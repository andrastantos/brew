from silicon import *


class UartParityType(Enum):
    none = 0
    even = 1
    odd  = 2

class UartStopBits(Enum):
    one = 0
    one_and_half = 1
    two = 2

class PhyDataIf(ReadyValid):
    data = Unsigned(8)

class UartTxPhy(Module):
    clk = ClkPort()
    rst = RstPort()

    ser_clk_en = Input(logic)

    data_in = Input(PhyDataIf)

    parity = Input(EnumNet(UartParityType))
    stop_cnt = Input(EnumNet(UartStopBits))
    word_size = Input(Unsigned(3))

    txd = Output(logic)

    def body(self):
        fsm = FSM()

        class UartTxPhyStates(Enum):
            idle = 0
            start = 1
            data = 2
            parity = 3
            stop_half = 4
            stop = 5

        fsm.reset_value <<= UartTxPhyStates.idle
        fsm.default_state <<= UartTxPhyStates.idle

        state = Wire()
        state <<= fsm.state

        oversampler = Wire(Unsigned(4))
        oversampler <<= Reg(Select(state == UartTxPhyStates.idle, (oversampler+1)[3:0], 0), clock_en=self.ser_clk_en)
        baud_tick = oversampler == 0
        baud_half_tick = oversampler == 8

        bit_counter = Wire(Unsigned(3))

        starting = self.data_in.ready & self.data_in.valid
        self.data_in.ready <<= (state == UartTxPhyStates.idle) & self.ser_clk_en

        bit_counter <<= Reg(Select(
            starting,
            Select(
                baud_tick,
                bit_counter,
                (bit_counter -1)[2:0]
            ),
            self.word_size
        ))
        shift_reg = Wire(Unsigned(8))
        shift_reg <<= Reg(
            Select(
                starting,
                Select(
                    baud_tick & (state == UartTxPhyStates.data),
                    shift_reg,
                    shift_reg >> 1
                ),
                self.data_in
            )
        )

        parity_reg = Wire(logic)
        parity_reg <<= Reg(
            Select(
                starting,
                Select(
                    baud_tick,
                    parity_reg,
                    parity_reg ^ shift_reg[0]
                ),
                (self.parity == UartParityType.odd)
            )
        )

        fsm.add_transition(UartTxPhyStates.idle, starting, UartTxPhyStates.start)
        fsm.add_transition(UartTxPhyStates.start, baud_tick, UartTxPhyStates.data)
        fsm.add_transition(UartTxPhyStates.data, (bit_counter == 0) & (self.parity == UartParityType.none) & baud_tick, UartTxPhyStates.stop)
        fsm.add_transition(UartTxPhyStates.data, (bit_counter == 0) & (self.parity != UartParityType.none) & baud_tick, UartTxPhyStates.parity)
        fsm.add_transition(UartTxPhyStates.parity, baud_tick & (self.stop_cnt == UartStopBits.one), UartTxPhyStates.idle)
        fsm.add_transition(UartTxPhyStates.parity, baud_tick & (self.hstop_cnt == UartStopBits.one_and_half), UartTxPhyStates.stop_half)
        fsm.add_transition(UartTxPhyStates.parity, baud_tick & (self.hstop_cnt == UartStopBits.two), UartTxPhyStates.stop)
        fsm.add_transition(UartTxPhyStates.stop_half, baud_half_tick, UartTxPhyStates.idle)
        fsm.add_transition(UartTxPhyStates.stop, baud_tick, UartTxPhyStates.idle)

        self.txd = Reg(Select(
            state == UartTxPhyStates.start, 0,
            state == UartTxPhyStates.data, shift_reg[0],
            state == UartTxPhyStates.parity, parity_reg,
            default_port = 1
        ))


class UartRxPhy(Module):
    clk = ClkPort()
    rst = RstPort()

    ser_clk_en = Input(logic)

    data_out = Output(PhyDataIf)

    parity = Input(EnumNet(UartParityType))
    stop_cnt = Input(EnumNet(UartStopBits))
    word_size = Input(Unsigned(3))

    rxd = Input(logic)

    framing_error = Output(logic)
    parity_error = Output(logic)
    overrun_error = Output(logic)
    clear = Input(logic)

    def body(self):
        fsm = FSM()

        class UartRxPhyStates(Enum):
            idle = 0
            half_start = 6
            start = 1
            data = 2
            parity = 3
            stop_half = 4
            stop = 5

        fsm.reset_value <<= UartRxPhyStates.idle
        fsm.default_state <<= UartRxPhyStates.idle

        state = Wire()
        state <<= fsm.state

        oversampler = Wire(Unsigned(4))
        oversampler <<= Reg(
            SelectOne(
                state == UartRxPhyStates.idle, 0,
                state == UartRxPhyStates.half_start, (oversampler+1)[2:0],
                state == UartRxPhyStates.stop_half, (oversampler+1)[2:0],
                default_port = (oversampler+1)[3:0]
            ),
            clock_en=self.ser_clk_en
        )
        baud_tick = oversampler == 0

        bit_counter = Wire(Unsigned(3))

        starting = ~self.data_out.ready & (state == UartRxPhyStates.idle) & ~rxd & ~self.framing_error & ~self.parity_error
        rx_full = Wire(logic)
        rx_full <<= Reg(
            Select(
                (state == UartRxPhyStates.parity) & baud_tick,
                Select(
                    self.data_out.ready,
                    rx_full,
                    0
                ),
                1
            )
        )
        self.data_in.valid <<= rx_full

        bit_counter <<= Reg(Select(
            starting,
            Select(
                baud_tick,
                bit_counter,
                (bit_counter -1)[2:0]
            ),
            self.word_size
        ))
        shift_reg = Wire(Unsigned(8))
        shift_reg <<= Reg(
            Select(
                starting,
                Select(
                    baud_tick & (state == UartRxPhyStates.data),
                    shift_reg,
                    concat(self.rx, shift_reg[6:0])
                ),
                self.data_in
            )
        )

        ref_parity_reg = Wire(logic)
        ref_parity_reg <<= Reg(
            Select(
                starting,
                Select(
                    baud_tick,
                    ref_parity_reg,
                    ref_parity_reg ^ rxd
                ),
                (self.parity == UartParityType.odd)
            )
        )

        self.parity_error <<= Reg(
            Select(
                self.clear,
                Select(
                    (state == UartRxPhyStates.parity) & baud_tick,
                    self.parity_error,
                    self.parity_error | (rxd != ref_parity_reg)
                ),
                0
            )
        )
        self.framing_error <<= Reg(
            Select(
                self.clear,
                Select(
                    ((state == UartRxPhyStates.stop_half) | (state == UartRxPhyStates.stop)) & (rxd == 0),
                    self.framing_error,
                    1
                ),
                0
            )
        )
        self.overrun_error <<= Reg(
            Select(
                self.clear,
                Select(
                    (state == UartRxPhyStates.start) & (rxd == 0) & rx_full,
                    self.overrun_error,
                    1,
                ),
                0
            )
        )

        fsm.add_transition(UartRxPhyStates.idle, starting, UartRxPhyStates.half_start)
        fsm.add_transition(UartRxPhyStates.half_start, baud_tick, UartRxPhyStates.start)
        fsm.add_transition(UartRxPhyStates.start, baud_tick, UartRxPhyStates.data)
        fsm.add_transition(UartRxPhyStates.data, (bit_counter == 0) & (self.parity == UartParityType.none) & baud_tick, UartRxPhyStates.stop)
        fsm.add_transition(UartRxPhyStates.data, (bit_counter == 0) & (self.parity != UartParityType.none) & baud_tick, UartRxPhyStates.parity)
        fsm.add_transition(UartRxPhyStates.parity, baud_tick & (self.stop_cnt == UartStopBits.one), UartRxPhyStates.idle)
        fsm.add_transition(UartRxPhyStates.parity, baud_tick & (self.stop_cnt == UartStopBits.one_and_half), UartRxPhyStates.stop_half)
        fsm.add_transition(UartRxPhyStates.parity, baud_tick & (self.stop_cnt == UartStopBits.two), UartRxPhyStates.stop)
        fsm.add_transition(UartRxPhyStates.stop_half, baud_tick, UartRxPhyStates.idle)
        fsm.add_transition(UartRxPhyStates.stop, baud_tick, UartRxPhyStates.idle)

        # CDC crossing
        rxd = Reg(Reg(self.rxd))
