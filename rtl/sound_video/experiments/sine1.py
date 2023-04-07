#!/bin/python
import time

import numpy as np
import pyaudio
from math import *
from typing import Sequence
import matplotlib.pyplot as plt
from enum import Enum

"""
We're building a data-flow graph, and executing it. The nodes of the graph are 'Procedure's. These procedures have:
- Some control inputs (these are input nodes on the graph)
- Some outputs (these are output nodes on the graph)
- Some parameters (these are static settings that can change, but not through other procedures)
- Some state (such as phase accumulators)

The graph is a DAG, except for one special 'procedure', a 'Delay' that puts the input on the output one evaluation cycle later.
Sort of a register in RTL...

There could be multiple drivers for a single control input, in which case, all inputs are summed.

Control inputs measure values between ~0 and 1 and are on a logarithmic scale.

The following procedures are needed:

Playback: this is the root of the graph. It is interfacing to the audio HW

Control parameters are 12 bit large. Not all controls support the full range of inputs.

Oscillator:
- Frequency control input. Logarithmically scaled from 16Hz to 8000Hz in 10th of a semitone steps
- Amplitude control input. Logarithmically scaled from -120dB to 0dB in 0.1dB steps
- Modulator control input. Linearly scaled from -2048% to 2048% of the frequency control input in 1% steps
  TODO: is this the right range? 10% is too big a step-size, but maybe we want bigger modulation range...
- Phase control input. Linearly scaled from 0 to 2pi in pi/2048 steps
- Output: output samples between -1 and 1, in 1/2048 steps
- Parameters:
  - wave-form selection
  - fine-tune (if needed)
NOTE: we need a linear modulation input, otherwise the modulator would not cancel out in a single wave period
      and the resulting waveform would not be repetitive.

Constant:
- Output: a parameter between -2048 and 2048
- Paramteres: the actual constant value

Envelope generator:
- Output: value between 0 and 1, in 1/2048 steps
- Parameters: ADSR values, start/stop triggers

LFO:
- Frequency control input. Logarithmically scaled from 0.01Hz to 20Hz in ??? steps
- Amplitude control input. Logarithmically scaled from -120dB to 0dB in 0.1dB steps
- Output: output samples between 0 and 1
- Parameters:
  - wave-form selection

Filter:
- Q control input. Logarithmically scaled from -120dB to ???dB in 0.1dB steps
- cut-off control input. Logarithmically scaled from 16Hz to 8000Hz in 10th of a semitone steps
- Parameters:
  - filter mod (low-pass, high-pass, band-pass, notch)

Pan:
- Input: samples between -1 and 1 in 1/2048 steps
- Pan control: **LINEARLY** scaled between left (-1) and right (1) in 1/2048 steps
- OutputL: left samples between -1 and 1 in 1/2048 steps
- OutputR: right samples between -1 and 1 in 1/2048 steps

Amplifier (not sure if needed):
- Input: samples between -1 and 1 in 1/2048 steps
- Amplitude control input. Logarithmically scaled from -120dB to 0dB in 0.1dB steps
- Output: samples between -1 and 1 in 1/2048 steps

Wavetable:
- Frequency control input. Logarithmically scaled from ???Hz to ???Hz in ??? steps
- Amplitude control input. Logarithmically scaled from -120dB to 0dB in 0.1dB steps
- Output: samples between -1 and 1 in 1/128 steps (left-adjusted, so bottom 4 bits are 0 or something)
- Parameters:
  - wave-table location
  - repeat points

In HW connectivity can be imagined as a large(ish) array, where each entity has a
place to pick their data from and a set of places where the deposit their data.

In each iteration:
1. The whole array is zeroed out
2. The graph is executed (from leafs to root(s)), each time outputs ADD-ing to the values in the array
3. The last thing to execute are the Playback entities which simply take their input values and put them on the DACs

This whole thing can be nicely scripted up, especially if the number of updates per entity is limited.
One can even imagine a small ISA that does all this. The program would be just a set of nodes to be
executed (already in the correct order) with a pointer to their state, which would contain all the
parameters, state variables and output pointers. One would need to be able to execute probably around
200 of these procedures per sample to make a reasonable capable synthesizer. That would (at 30kHz
sampling rate) be 6M procedures per second. No issues these days, but probably prohibitive back then,
especially considering the complexity of each of these procedures.

If each procedure has 8 words worth of state (some might have more, some probably less), that would be 16 bytes,
or more than 3kByte of state storage. The connectivity array would be about 800 bytes large. So, a total
of 4kByte of memory needed, but that could contain the 'program' as well. Now, that's not unheard of,
but as an external SRAM, something that we don't have pins for.

So... Damn!

Still, I might do this project just for the fun of it. At least, the initial Python version.

So, actually we have a hope if we're OK to compromise on 50 procedures and 15kHz sampling rate.

1. Quickly converting logarithmic inputs to linear scale
---------------------------------------------------------

First of all, to represent 8 octaves of tones in 0.1 semit-tone resolution, 10 bits is sufficient.
10 bits is also sufficient to represent 96dB dynamic range in ~0.1dB resolution.

So, the question is: how to quickly compute the frequency from a 10-bit logarithmic number of such.

1. Let's call the input as: I, the output as O
2. Get the top 3 bits of I: I[9:7], call it E. Look up a correction factor (C) and add it to I[6:0].
   Call the result of the summation M.
3. If M > 119, increment E and subtract 120 from M. At this point E is in powers of 2 and M is a
   still-logarithmic mantissa.
4. Use two look-ups, to get a value for the top 4 and the bottom 3 bits of M:
   M1 = lookup1(M[2:0]), M2 = lookup2(M[6:3])
5. Multiply M1 and M2 together (with proper fractional bit-selection) to get M'. This is the now
   linear mantissa.
6. Compute the final result by shifting M' by E bits.

A similar approach can be used for amplitude conversion, except:
1. Take the top four bits as E, use no correction for M
2. Use (different) dual-3-bit lookups

The frequency value can be directly used as a phase-increment for the phase-accumulator of oscillators
and wave-tables. (Not sure about filters.)

However, these procedures would be better off with bresenham specification of phase-accumulators.
Which is convenient as the two lookups in (4) give exactly that representation. We can use M1 as
horizontal increment and 2^E*M2 as the vertical one. This allows multiply-free phase-accumulation!!!!!

Bresanham method requires that 2^E*M2 <= M1. If not, the first needs to be represented as a quotient
and a remainder. The quotient is used as an integer increment for the phase accumulator and the
remainder is used as the fractional increment (module M1). So, didn't we just buy ourselves a divide
instead of a multiply?

I don't think multiplies are avoidable for amplitude controls. In fact, another multiply is needed
for the application of the linearized value.
"""

volume = 0.5  # range [0.0, 1.0]
fs = 44100  # sampling rate, Hz, must be integer
duration = 2  # in seconds, mist be integer
f0 = 440.0  # sine frequency, Hz, may be float

params: Sequence['Param'] = []

class Param(object):
    def __init__(self, value = None):
        global params
        self._value = value
        params.append(self)

    def update(self,value):
        self._value += value

    def reset(self):
        self._value = 0

    @property
    def value(self):
        return self._value

procedures = []

class Const(object):
    def __init__(self, value):
        global procedures
        procedures.append(self)

        self.out = Param()
        self.value = value

    def next(self):
        self.out.update(self.value)

class Ramp(object):
    def __init__(self, value, slope):
        global procedures
        procedures.append(self)

        self.out = Param()
        self.value = value
        self.slope = slope

    def next(self):
        self.out.update(self.value)
        self.value += self.slope/fs

class WaveForms(Enum):
    sine = 0
    triangle = 1
    sawtooth = 2
    square = 3

class Oscillator(object):
    def __init__(self, waveform: WaveForms, initial_phase=0):
        global procedures
        procedures.append(self)

        self.in_freq = None
        self.in_ampl = None
        self.in_fmod = None
        self.waveform = waveform
        self.out = Param()
        self._phase = initial_phase


    def next(self):
        global params
        # Frequency is measured in 1/10th of a semitone. 0 corresponds to middle-A (440Hz)
        freq_in_hz = f0 * 2.0**(self.in_freq.value/120)
        if self.in_fmod is not None:
            # fmod is specified in % of the above
            freq_in_hz *= 1+int(self.in_fmod.value)/100
        # Amplitude is measured on 0.1dB, 0dB corresponding to an amplitude of '1'
        lin_ampl = 10.0**(self.in_ampl.value/100)
        try:
            phase_increment = 2*pi/(fs/freq_in_hz)
        except ZeroDivisionError:
            phase_increment = 0
        if self.waveform == WaveForms.sine:
            a = sin(self._phase)
        elif self.waveform == WaveForms.sawtooth:
            a = self._phase/pi - 1
        elif self.waveform == WaveForms.triangle:
            a = abs(self._phase/pi*2 - 2) - 1
        elif self.waveform == WaveForms.square:
            a = 1 if self._phase > pi else -1
        self.out.update(lin_ampl * a)
        self._phase += phase_increment
        while self._phase > 2*pi: self._phase -= 2*pi

# generate samples, note conversion to float32 array
#samples = (np.sin(2 * np.pi * np.arange(fs * duration) * f / fs)).astype(np.float32)

note_freq = -240
note = Const(note_freq)
#note2 = Ramp(0,60)
ampl = Const(-100)

modulator_freq = Const(note_freq)
modulator_ampl = Const(200)

modulator = Oscillator(WaveForms.sine, initial_phase=pi)
osc = Oscillator(WaveForms.sine)

modulator.in_ampl = modulator_ampl
modulator.in_freq = modulator_freq

osc.in_ampl = ampl.out
osc.in_freq = note.out
#note2.out = osc.in_freq
#modulator.out = osc.in_freq
osc.in_fmod = modulator.out

output = osc.out
samples = []
for i in range(fs*duration):
    for p in params:
        p.reset()
    note.next()
    #note2.next()
    ampl.next()
    modulator.next()
    osc.next()
    samples.append(output.value)

# per @yahweh comment explicitly convert to bytes sequence
output_bytes = np.array(samples, dtype="float32").tobytes()

# for paFloat32 sample values must be in range [-1.0, 1.0]
p = pyaudio.PyAudio()

stream = p.open(
    format=pyaudio.paFloat32,
    channels=1,
    rate=fs,
    output=True
)

# play. May repeat with different volume values (if done interactively)
start_time = time.time()
stream.write(output_bytes)
print("Played sound for {:.2f} seconds".format(time.time() - start_time))

stream.stop_stream()
stream.close()

p.terminate()

section = samples[0:1500]
plt.plot(np.arange(0, len(section)/fs, 1/fs), section)
plt.show(block=True)