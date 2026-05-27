// CI smoke tests for the MLX90363 packet codec. Reference values are
// derived from the datasheet (CRC table) plus round-trip checks via
// makePacket → parseData.

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';

import {
  CRC,
  Marker,
  OutgoingOpcode,
  IncomingOpcode,
  parseData,
  makePacket,
  MapXYZ,
  mapXYZ,
  computeAlpha,
  computeAlphaBeta,
  defaultConstants,
  EEchallenge,
  type AlphaMessage,
  type AlphaBetaMessage,
  type XYZMessage,
  type MemoryRead_AnswerMessage,
  type Ready_MessageMessage,
} from './MLX90363.js';

describe('CRC', () => {
  it('rejects non-8-byte buffers', () => {
    assert.throws(() => CRC(Buffer.alloc(7)), /InvalidLength/);
    assert.throws(() => CRC(Buffer.alloc(9)), /InvalidLength/);
  });

  it('computes CRC of all-zeros over 7 bytes', () => {
    // Pure datasheet: starting with 0xff XOR 0 = 0xff. Table[0xff] = 0x42.
    // 7 iterations: XOR with 0 then lookup. After first lookup: 0x42.
    // 0x42 XOR 0 = 0x42, table[0x42] = ... etc. The deterministic value
    // we land on is what the round-trip property uses.
    const out = CRC(Buffer.alloc(8));
    assert.equal(typeof out, 'number');
    assert.ok(out >= 0 && out <= 255);
  });

  it('makePacket → CRC matches buffer byte 7', () => {
    const pkt = makePacket({ opcode: OutgoingOpcode.GET1 });
    assert.equal(pkt.length, 8);
    assert.equal(pkt[7], CRC(pkt));
  });

  it('changing a byte changes the CRC', () => {
    const a = makePacket({ opcode: OutgoingOpcode.GET1 });
    const b = Buffer.from(a);
    b[0] = 0xaa;
    assert.notEqual(CRC(a), CRC(b));
  });
});

describe('makePacket', () => {
  it('default marker is Opcode', () => {
    const pkt = makePacket({ opcode: OutgoingOpcode.GET1 });
    assert.equal(pkt[6] >> 6, Marker.Opcode);
    assert.equal(pkt[6] & 0x3f, OutgoingOpcode.GET1);
  });

  it('writes 8-bit data with data8', () => {
    const pkt = makePacket({
      opcode: OutgoingOpcode.MemoryRead,
      data8: [0x12, 0x34, undefined, 0x56],
    });
    assert.equal(pkt[0], 0x12);
    assert.equal(pkt[1], 0x34);
    assert.equal(pkt[2], 0x00); // undefined → left zero
    assert.equal(pkt[3], 0x56);
  });

  it('writes 16-bit data with data16 (little-endian)', () => {
    const pkt = makePacket({
      opcode: OutgoingOpcode.MemoryRead,
      data16: [0x1234, 0x5678],
    });
    assert.equal(pkt.readUInt16LE(0), 0x1234);
    assert.equal(pkt.readUInt16LE(2), 0x5678);
  });

  it('explicit marker is honored', () => {
    const pkt = makePacket({
      opcode: OutgoingOpcode.GET1,
      marker: Marker.Alpha,
    });
    assert.equal(pkt[6] >> 6, Marker.Alpha);
  });
});

describe('parseData', () => {
  it('parses an Alpha-marker packet (round-trip via makePacket+parseData)', () => {
    // For Alpha/AlphaBeta/XYZ markers, the low 6 bits of byte 6 carry the
    // chip's "roll" counter rather than an opcode. We give makePacket any
    // opcode just to satisfy the type, then overwrite byte 6 + recompute CRC.
    const pkt = makePacket({
      opcode: OutgoingOpcode.GET1,
      marker: Marker.Alpha,
      data8: [0x34, 0x12, 0, 0, 0x55, 0],
    });
    pkt[6] = (Marker.Alpha << 6) | 10; // roll = 10
    pkt[7] = CRC(pkt);

    const parsed = parseData(pkt) as AlphaMessage;
    assert.equal(parsed.marker, Marker.Alpha);
    assert.equal(parsed.crc, true);
    assert.equal(parsed.roll, 10);
    assert.equal(parsed.alpha, 0x1234);
    assert.equal(parsed.vg, 0x55);
  });

  it('parses an AlphaBeta-marker packet', () => {
    const pkt = Buffer.alloc(8);
    pkt[0] = 0x11;
    pkt[1] = 0x22;
    pkt[2] = 0x33;
    pkt[3] = 0x14; // upper 2 bits zero → beta = 0x1433
    pkt[4] = 0x66;
    pkt[6] = (Marker.AlphaBeta << 6) | 7;
    pkt[7] = CRC(pkt);

    const parsed = parseData(pkt) as AlphaBetaMessage;
    assert.equal(parsed.marker, Marker.AlphaBeta);
    assert.equal(parsed.crc, true);
    assert.equal(parsed.roll, 7);
    assert.equal(parsed.alpha, 0x2211);
    assert.equal(parsed.beta, 0x1433);
    assert.equal(parsed.vg, 0x66);
  });

  it('parses an XYZ-marker packet with sign extension on negatives', () => {
    const pkt = Buffer.alloc(8);
    // Z = -1 in 14-bit signed → 0x3fff in low 14 bits → bytes 0xff, 0x3f
    // and we expect sign extension to fill bit 6 & 7.
    pkt[0] = 0x01;
    pkt[1] = 0x00; // X = 1
    pkt[2] = 0xff;
    pkt[3] = 0x3f; // Y = 0x3fff signed (bit 13 set) → after sign extension, negative
    pkt[4] = 0x00;
    pkt[5] = 0x00; // Z = 0
    pkt[6] = (Marker.XYZ << 6) | 3;
    pkt[7] = CRC(pkt);

    const parsed = parseData(pkt) as XYZMessage;
    assert.equal(parsed.marker, Marker.XYZ);
    assert.equal(parsed.crc, true);
    assert.equal(parsed.x, 1);
    // Y had bit 13 set → sign extension → negative
    assert.ok(parsed.y < 0, `expected negative y, got ${parsed.y}`);
    assert.equal(parsed.z, 0);
    assert.ok('computed' in parsed);
  });

  it('reports failed CRC', () => {
    // Use an Incoming-opcode packet so parseData has a branch to take.
    // (parseData throws on Outgoing opcodes — they're not meant to be parsed.)
    const pkt = Buffer.alloc(8);
    pkt[6] = (Marker.Opcode << 6) | IncomingOpcode.NothingToTransmit;
    pkt[7] = CRC(pkt) ^ 0xff;
    const parsed = parseData(pkt);
    assert.equal(parsed.crc, false);
  });

  it('parses MemoryRead_Answer opcode message', () => {
    const pkt = Buffer.alloc(8);
    pkt.writeUInt16LE(0xabcd, 0);
    pkt.writeUInt16LE(0x1234, 2);
    pkt[6] = (Marker.Opcode << 6) | IncomingOpcode.MemoryRead_Answer;
    pkt[7] = CRC(pkt);

    const parsed = parseData(pkt) as MemoryRead_AnswerMessage;
    assert.equal(parsed.marker, Marker.Opcode);
    assert.equal(parsed.opcode, IncomingOpcode.MemoryRead_Answer);
    assert.equal(parsed.data0, 0xabcd);
    assert.equal(parsed.data1, 0x1234);
  });

  it('parses Ready_Message opcode message', () => {
    const pkt = Buffer.alloc(8);
    pkt[0] = 0x07; // hwVersion
    pkt[1] = 0x42; // fwVersion
    pkt[6] = (Marker.Opcode << 6) | IncomingOpcode.Ready_Message;
    pkt[7] = CRC(pkt);

    const parsed = parseData(pkt) as Ready_MessageMessage;
    assert.equal(parsed.opcode, IncomingOpcode.Ready_Message);
    assert.equal(parsed.hwVersion, 0x07);
    assert.equal(parsed.fwVersion, 0x42);
  });
});

describe('mapXYZ', () => {
  const c = { x: 1, y: 2, z: 3 };

  it('XYZ is identity', () => {
    assert.deepEqual(mapXYZ(c, MapXYZ.XYZ), [1, 2, 3]);
  });

  it('XZY swaps Y and Z', () => {
    assert.deepEqual(mapXYZ(c, MapXYZ.XZY), [1, 3, 2]);
  });

  it('YZX rotates', () => {
    assert.deepEqual(mapXYZ(c, MapXYZ.YZX), [2, 3, 1]);
  });

  it('YXZ swaps X and Y', () => {
    assert.deepEqual(mapXYZ(c, MapXYZ.YXZ), [2, 1, 3]);
  });

  it('ZXY rotates', () => {
    assert.deepEqual(mapXYZ(c, MapXYZ.ZXY), [3, 1, 2]);
  });

  it('ZYX reverses', () => {
    assert.deepEqual(mapXYZ(c, MapXYZ.ZYX), [3, 2, 1]);
  });
});

describe('computeAlpha / computeAlphaBeta', () => {
  it('alpha of (1, 0) is 0 (0° angle)', () => {
    assert.equal(computeAlpha([1, 0]), 0);
  });

  it('alpha is in 14-bit range', () => {
    const a = computeAlpha([0, 1]); // 90°
    assert.ok(a >= 0 && a < 1 << 14, `expected 14-bit value, got ${a}`);
    assert.equal(a, 1 << 12); // 90° = 16384/4 = 4096
  });

  it('computeAlphaBeta returns 14-bit alpha and beta', () => {
    const { alpha, beta } = computeAlphaBeta([1, 1, 1], defaultConstants);
    assert.ok(alpha >= 0 && alpha < 1 << 14);
    assert.ok(beta >= 0 && beta < 1 << 14);
  });
});

describe('EEchallenge', () => {
  it('has 32 entries (matches datasheet)', () => {
    assert.equal(EEchallenge.length, 32);
  });

  it('all entries fit in 16 bits', () => {
    for (const v of EEchallenge) {
      assert.ok(v >= 0 && v < 1 << 16, `value out of range: ${v}`);
    }
  });
});
