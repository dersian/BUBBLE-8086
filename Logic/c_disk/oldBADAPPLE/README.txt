This code is a small monochrome video decoder written for the 80386 in DOS.
It needs two data files to run:
- "BAMUSIC.RAW", a headerless WAV-file with 8-bit PCM and 22050Hz sampling rate
- "VIDFILE.BIN" containing the encoded videostream

Frames are differentially encoded, where every subsequent decoded frame is XOR'ed with the current buffer. It can also signal negated frame (all white pixels become balck and vice versa). Frames are divided into 8x8 blocks, which are either designated "empty" (no change) or "data" (some change has happened).
The number of blocks are signalled with Exponential-Golomb coding, while the bytes (8 per block) with the difference information are Huffman-encoded.

For more information, see the source code, or mail me at david.blinder@vub.be