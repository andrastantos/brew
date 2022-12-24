Interpolation
-------------

I'm actually not sure at all what I wanted the interpolation instruction to do anymore.

The simulation just throws an exception and this old description below describes something woefully complex.

2D interpolation
~~~~~~~~~~~~~~~~

Inputs:

#. Base and stride register. The lower 4 bits determine output (and texture data) type. The next 4 bits determine the stride as a power of 2, such that value 0 is a stride of 2 and value 15 is 65536. Stride is measured in 32-bit
quantities. The top 24 bits determine the 256-byte aligned base address for the texture.
#. Coordinates. Encoded in (U)FR12P4x2 format, it's two unsigned elements denote the fractional coordinates within the texture
#. Element type can be the following::

        SINT32     - ?
        UINT32     - ?
        SINT16     - mono audio processing
        UINT16     - ?
        SINT8      - bump maps
        UINT8      - mono images
        SINT16X2   - stereo audio processing
        UINT16X2   - ?
        SINT8X4    - ?
        UINT8X4    - RGBA images
   NOTE: element types are different from register types due to the
         different needs.

Operation:

#. The four neighbor coordinates are computed and their four 32-bit values are loaded.
#. Interpolation is performed between the four values (lane-by-lane) using the fractional values of the coordinates. By the nature of the operation, the results can't overflow, but signed-ness matter
#. The result is written into the output register

Output:

#. The interpolated value (lane-by-lane)

Overall, this is a binary operation, albeit one that involves 4 memory reads as well. As such, it's anything but a RISC operation. Probably, just as others have done it, it should be a tack-on execution unit.

Another (not much better) way of dealing with the problem is to say that the base/stride registers' lowest 2 bits describe the output type, but that would mean that the 2D alignment stride will become 64-byte aligned, because the bottom 6 bits are now used for other stuff.

At the same time, maybe other operations could also benefit from looking at the result type? Well, here's why they shouldn't: That effectively creates a 3rd read port into the register file! Our BRAMs are organized as 36-bit wide, so the type info comes with the value. Now, how would we know the result type??

Maybe even multi-cycle, instead of fully pipelined?

1D interpolation
~~~~~~~~~~~~~~~~

Inputs:

#. Base register. This now simply is a 32-bit DWORD-aligned pointer.
#. Coordinate: Encoded as a (U)FR24P8 value.
#. Element type. Supported values::

        INT32 (signed interpolation),
        INT16X2 (signed interpolation),
        INT8X2 (unsigned interpolation)

Operation:

#. The two neighbor coordinates are computed and their 32-bit values are loaded
#. Interpolation is performed lane-by-lane.
#. The result register is written

Output:

#. The interpolated value (lane-by-lane)

A single binary opcode could be used to encode all of this, because the type of the coordinate register can be used to determine the 1D/2D interpolation and the result register type could be used to determine the interpolation type.

This is a bit unusual though: most operations *determine* the output type, while this one requires it to be set a priori.

