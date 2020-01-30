// prettier-ignore
/**
 * Matches datasheet
 */
const cba_256_TAB = [
  0x00, 0x2f, 0x5e, 0x71, 0xbc, 0x93, 0xe2, 0xcd,
  0x57, 0x78, 0x09, 0x26, 0xeb, 0xc4, 0xb5, 0x9a,
  0xae, 0x81, 0xf0, 0xdf, 0x12, 0x3d, 0x4c, 0x63,
  0xf9, 0xd6, 0xa7, 0x88, 0x45, 0x6a, 0x1b, 0x34,
  0x73, 0x5c, 0x2d, 0x02, 0xcf, 0xe0, 0x91, 0xbe,
  0x24, 0x0b, 0x7a, 0x55, 0x98, 0xb7, 0xc6, 0xe9,
  0xdd, 0xf2, 0x83, 0xac, 0x61, 0x4e, 0x3f, 0x10,
  0x8a, 0xa5, 0xd4, 0xfb, 0x36, 0x19, 0x68, 0x47,
  0xe6, 0xc9, 0xb8, 0x97, 0x5a, 0x75, 0x04, 0x2b,
  0xb1, 0x9e, 0xef, 0xc0, 0x0d, 0x22, 0x53, 0x7c,
  0x48, 0x67, 0x16, 0x39, 0xf4, 0xdb, 0xaa, 0x85,
  0x1f, 0x30, 0x41, 0x6e, 0xa3, 0x8c, 0xfd, 0xd2,
  0x95, 0xba, 0xcb, 0xe4, 0x29, 0x06, 0x77, 0x58,
  0xc2, 0xed, 0x9c, 0xb3, 0x7e, 0x51, 0x20, 0x0f,
  0x3b, 0x14, 0x65, 0x4a, 0x87, 0xa8, 0xd9, 0xf6,
  0x6c, 0x43, 0x32, 0x1d, 0xd0, 0xff, 0x8e, 0xa1,
  0xe3, 0xcc, 0xbd, 0x92, 0x5f, 0x70, 0x01, 0x2e,
  0xb4, 0x9b, 0xea, 0xc5, 0x08, 0x27, 0x56, 0x79,
  0x4d, 0x62, 0x13, 0x3c, 0xf1, 0xde, 0xaf, 0x80,
  0x1a, 0x35, 0x44, 0x6b, 0xa6, 0x89, 0xf8, 0xd7,
  0x90, 0xbf, 0xce, 0xe1, 0x2c, 0x03, 0x72, 0x5d,
  0xc7, 0xe8, 0x99, 0xb6, 0x7b, 0x54, 0x25, 0x0a,
  0x3e, 0x11, 0x60, 0x4f, 0x82, 0xad, 0xdc, 0xf3,
  0x69, 0x46, 0x37, 0x18, 0xd5, 0xfa, 0x8b, 0xa4,
  0x05, 0x2a, 0x5b, 0x74, 0xb9, 0x96, 0xe7, 0xc8,
  0x52, 0x7d, 0x0c, 0x23, 0xee, 0xc1, 0xb0, 0x9f,
  0xab, 0x84, 0xf5, 0xda, 0x17, 0x38, 0x49, 0x66,
  0xfc, 0xd3, 0xa2, 0x8d, 0x40, 0x6f, 0x1e, 0x31,
  0x76, 0x59, 0x28, 0x07, 0xca, 0xe5, 0x94, 0xbb,
  0x21, 0x0e, 0x7f, 0x50, 0x9d, 0xb2, 0xc3, 0xec,
  0xd8, 0xf7, 0x86, 0xa9, 0x64, 0x4b, 0x3a, 0x15,
  0x8f, 0xa0, 0xd1, 0xfe, 0x33, 0x1c, 0x6d, 0x42,
];

// prettier-ignore
/**
 * Matches datasheet
 */
export const EEchallenge = [
  17485, 31053, 57190, 57724,  7899, 53543, 26763, 12528,
  38105, 51302, 16209, 24847, 13134, 52339, 14530, 18350,
  55636, 64477, 40905, 45498, 24411, 36677,  4213, 48843,
   6368,  5907, 31384, 63325,  3562, 19816,  6995,  3147,
];

export function CRC(data: Buffer) {
  if (data.length != 8) throw new Error('InvalidLength');

  let crc = 0xff;
  for (let i = 0; i < 7; i++) crc = cba_256_TAB[data[i] ^ crc];

  return ~crc & 0xff;
}

export enum Marker {
  Alpha,
  AlphaBeta,
  XYZ,
  Opcode,
}

// prettier-ignore
/**
 * Opcodes that we expect in Outgoing messages.
 */
export enum OutgoingOpcode {
  GET1              = 0x13,
  GET2              = 0x14,
  GET3              = 0x15,
  MemoryRead        = 0x01,
  EEPROMWrite       = 0x03,
  EEChallengeAns    = 0x05,
  EEReadChallenge   = 0x0f,
  NOP__Challenge    = 0x10,
  DiagnosticDetails = 0x16,
  OscCounterStart   = 0x18,
  OscCounterStop    = 0x1a,
  Reboot            = 0x2f,
  Standby           = 0x31,
}

// prettier-ignore
/**
 * Opcodes that we expect in Incoming messages.
 */
export enum IncomingOpcode {
  Get3Ready                      = 0x2d,
  MemoryRead_Answer              = 0x02,
  EEPROMWrite_Challenge          = 0x04,
  EEReadAnswer                   = 0x28,
  EEPROMWrite_Status             = 0x0e,
  Challenge__NOP_MISO_Packet     = 0x11,
  Diagnostics_Answer             = 0x17,
  OscCounterStart_Acknowledge    = 0x19,
  OscCounterStopAck_CounterValue = 0x1b,
  StandbyAck                     = 0x32,
  Error_frame                    = 0x3d,
  NothingToTransmit              = 0x3e,
  Ready_Message                  = 0x2c,
}

export type Opcode = OutgoingOpcode | IncomingOpcode;

export enum DiagnosticStatus {
  Init,
  Fail,
  Pass,
  PassNew,
}
export enum ErrorCode {
  IncorrectBitCount = 1,
  IncorrectCRC = 2,
  NTT = 3,
  OpcodeNotValid = 4,
}
export enum EECode {
  Success = 1,
  WriteFail = 2,
  CRCWriteFail = 4,
  KeyInvalid = 6,
  CallengeFail = 7,
  OddAddress = 8,
}

export interface CRC {
  crc: boolean;
}

export type Message<M extends Marker = Marker, T extends {} = {}> = {
  marker: M;
} & T &
  CRC;

export type NormalMarker = Marker.Alpha | Marker.AlphaBeta | Marker.XYZ;

export type NormalMessage<M extends NormalMarker, T extends {}> = Message<
  M,
  {
    roll: number;
    diagnosticStatus: DiagnosticStatus;
  } & T
>;

export type AlphaMessage = NormalMessage<
  Marker.Alpha,
  {
    vg: number;
    alpha: number;
  }
>;

export type AlphaBetaMessage = NormalMessage<
  Marker.AlphaBeta,
  {
    vg: number;
    alpha: number;
    beta: number;
  }
>;

export type Components = {
  x: number;
  y: number;
  z: number;
};

export type XYZMessage = NormalMessage<
  Marker.XYZ,
  Components & {
    /**
     * What we expect the device to calculate internally were it in different modes.
     *
     * Assumes default map (XYZ) and trimming parameters (1.2, 1.2, 1). (Subject to change)
     *
     * To calculate yourself, use `computeInternal()`, `computeAlpha()`, or `computeAlphaBeta()` with `mapXYZ()`
     */
    computed: ReturnType<typeof computeInternal>;
  }
>;

export type NormalMessages = AlphaMessage | AlphaBetaMessage | XYZMessage;

export type ReceivedOpcodeMessage<
  O extends IncomingOpcode,
  T extends {} = {}
> = Message<Marker.Opcode, { opcode: O } & T>;

export type MemoryRead_AnswerMessage = ReceivedOpcodeMessage<
  IncomingOpcode.MemoryRead_Answer,
  {
    data0: number;
    data1: number;
  }
>;

export type EEPROMWrite_ChallengeMessage = ReceivedOpcodeMessage<
  IncomingOpcode.EEPROMWrite_Challenge,
  {
    challengeKey: number;
  }
>;

export type EEReadAnswerMessage = ReceivedOpcodeMessage<
  IncomingOpcode.EEReadAnswer
>;

export type EEPROMWrite_StatusMessage = ReceivedOpcodeMessage<
  IncomingOpcode.EEPROMWrite_Status,
  {
    code: EECode;
  }
>;

export type Challenge__NOP_MISO_PacketMessage = ReceivedOpcodeMessage<
  IncomingOpcode.Challenge__NOP_MISO_Packet,
  {
    key: number;
    invertedKey: number;
  }
>;

export type Diagnostics_AnswerMessage = ReceivedOpcodeMessage<
  IncomingOpcode.Diagnostics_Answer
>;

export type OscCounterStart_AcknowledgeMessage = ReceivedOpcodeMessage<
  IncomingOpcode.OscCounterStart_Acknowledge
>;

export type OscCounterStopAck_CounterValueMessage = ReceivedOpcodeMessage<
  IncomingOpcode.OscCounterStopAck_CounterValue
>;

export type StandbyAckMessage = ReceivedOpcodeMessage<
  IncomingOpcode.StandbyAck
>;

export type Error_frameMessage = ReceivedOpcodeMessage<
  IncomingOpcode.Error_frame,
  {
    error: ErrorCode;
  }
>;

export type NothingToTransmitMessage = ReceivedOpcodeMessage<
  IncomingOpcode.NothingToTransmit
>;

export type Ready_MessageMessage = ReceivedOpcodeMessage<
  IncomingOpcode.Ready_Message,
  {
    hwVersion: number;
    fwVersion: number;
  }
>;

export type ReceivedOpcodeMessages =
  | MemoryRead_AnswerMessage
  | EEPROMWrite_ChallengeMessage
  | EEReadAnswerMessage
  | EEPROMWrite_StatusMessage
  | Challenge__NOP_MISO_PacketMessage
  | Diagnostics_AnswerMessage
  | OscCounterStart_AcknowledgeMessage
  | OscCounterStopAck_CounterValueMessage
  | StandbyAckMessage
  | Error_frameMessage
  | NothingToTransmitMessage
  | Ready_MessageMessage;

export type Messages = NormalMessages | ReceivedOpcodeMessages;

export function parseData(data: Buffer): Messages {
  const crc = CRC(data) == data[7];

  const marker: Marker = data[6] >> 6;
  const roll = data[6] & 0b111111;

  // Only valid for "normal" messages
  const diagnosticStatus: DiagnosticStatus = data[1] >> 6;

  switch (marker) {
    case Marker.Alpha:
      return {
        crc,
        roll,
        marker,
        vg: data[4],
        alpha: data.readUInt16LE(0) & 0x3fff,
        diagnosticStatus,
      };
    case Marker.AlphaBeta:
      return {
        crc,
        roll,
        marker,
        vg: data[4],
        alpha: data.readUInt16LE(0) & 0x3fff,
        beta: data.readUInt16LE(2) & 0x3fff,
        diagnosticStatus,
      };
    case Marker.XYZ:
      const b = Buffer.allocUnsafe(6);
      data.copy(b);

      for (let i = 0; i < 3; i++) {
        if (b[i * 2 + 1] & 0b100000) b[i * 2 + 1] |= 0b11000000;
        else b[i * 2 + 1] &= 0x3f;
      }

      const components = {
        x: b.readInt16LE(0),
        y: b.readInt16LE(2),
        z: b.readInt16LE(4),
      };

      return {
        crc,
        roll,
        marker,
        ...components,
        diagnosticStatus,
        computed: computeInternal(components),
      };
    case Marker.Opcode:
      const opcode: Opcode = roll;
      switch (opcode) {
        case OutgoingOpcode.GET1:
        case OutgoingOpcode.GET2:
        case OutgoingOpcode.GET3:
        case OutgoingOpcode.MemoryRead:
        case OutgoingOpcode.EEPROMWrite:
        case OutgoingOpcode.EEChallengeAns:
        case OutgoingOpcode.EEReadChallenge:
        case OutgoingOpcode.NOP__Challenge:
        case OutgoingOpcode.DiagnosticDetails:
        case OutgoingOpcode.OscCounterStart:
        case OutgoingOpcode.OscCounterStop:
        case OutgoingOpcode.Reboot:
        case OutgoingOpcode.Standby:
          // TODO: Parse these instead of throwing
          throw new Error('This is data sent TO device...');

        case IncomingOpcode.Get3Ready:
          throw new Error('Not yet implemented');

        case IncomingOpcode.MemoryRead_Answer:
          return {
            crc,
            opcode,
            marker,
            data0: data.readUInt16LE(0),
            data1: data.readUInt16LE(2),
          };

        case IncomingOpcode.EEPROMWrite_Challenge:
          return { crc, opcode, marker, challengeKey: data.readUInt16LE(2) };

        case IncomingOpcode.EEReadAnswer:
          return { crc, opcode, marker };

        case IncomingOpcode.EEPROMWrite_Status:
          const code: EECode = data[0];
          return { crc, opcode, marker, code };

        case IncomingOpcode.Challenge__NOP_MISO_Packet:
          return {
            crc,
            opcode,
            marker,
            key: data.readInt16LE(2),
            invertedKey: data.readInt16LE(4),
          };

        case IncomingOpcode.Diagnostics_Answer:
        case IncomingOpcode.OscCounterStart_Acknowledge:
        case IncomingOpcode.OscCounterStopAck_CounterValue:
        case IncomingOpcode.StandbyAck:
          throw new Error('Not yet implemented');

        case IncomingOpcode.Error_frame:
          const error: ErrorCode = data[0];
          return { crc, opcode, marker, error };

        case IncomingOpcode.NothingToTransmit:
          return { crc, opcode, marker };

        case IncomingOpcode.Ready_Message:
          return {
            crc,
            opcode,
            marker,
            hwVersion: data[0],
            fwVersion: data[1],
          };

        default:
          throw new Error('Invalid Opcode');
      }
  }
}

export type Packet = {
  opcode: Opcode;
  marker?: Marker;
  data8?: (undefined | number)[];
  data16?: (undefined | number)[];
};

export function makePacket(data: Packet) {
  const ret = Buffer.alloc(8);

  const marker = data.marker === undefined ? Marker.Opcode : data.marker;

  if (data.data8)
    data.data8.forEach((n, i) => {
      if (n !== undefined) ret[i] = n;
    });

  if (data.data16)
    data.data16.forEach((n, i) => {
      if (n !== undefined) ret.writeUInt16LE(n, i * 2);
    });

  ret[6] = (marker << 6) | data.opcode;

  ret[7] = CRC(ret);
  return ret;
}

export enum MapXYZ {
  XYZ = 0,
  XZY = 1,
  YZX = 2,
  YXZ = 3, // Use mode 0 instead
  ZXY = 4,
  ZYX = 5,
}

export function mapXYZ(
  { x, y, z }: Components,
  map: MapXYZ = 0
): [number, number, number] {
  switch (map) {
    default:
      throw new TypeError('Wrong map');
    case MapXYZ.XZY:
      return [x, z, y];
    case MapXYZ.XYZ:
      return [x, y, z];
    case MapXYZ.YZX:
      return [y, z, x];
    case MapXYZ.YXZ:
      return [y, x, z];
    case MapXYZ.ZXY:
      return [z, x, y];
    case MapXYZ.ZYX:
      return [z, y, x];
  }
}

export type Constants = { kAlpha: number; kBeta: number; kT: number };

export const defaultConstants: Constants = { kAlpha: 1.2, kBeta: 1.2, kT: 1 };

/**
 * Compute Alpha and Alpha+Beta from raw component value.
 *
 * Should perfectly match internal computation on MLX90363
 *
 * @param param0 Raw analog component values
 */
export function computeInternal(
  components: Components,
  map: MapXYZ = 0,
  constants = defaultConstants
) {
  const mapped = mapXYZ(components, map);
  return {
    alpha: computeAlpha(mapped),
    alphaBeta: computeAlphaBeta(mapped, constants),
  };
}

function scaleAngleToBits(angle: number, bits = 14): number {
  // Make sure we're in the positive range
  if (angle < 0) angle += Math.PI * 2;

  // Scale to n bits
  angle *= 2 ** bits / (Math.PI * 2);

  // Make sure result is integer
  return Math.floor(angle);
}

export function computeAlpha([B1, B2]:
  | [number, number]
  | [number, number, number]): number {
  return scaleAngleToBits(Math.atan2(B2, B1));
}

export function computeAlphaBeta(
  [B1, B2, B3]: [number, number, number],
  { kAlpha, kBeta, kT } = defaultConstants
): { alpha: number; beta: number } {
  const alphaNum = Math.sqrt((kAlpha * B3) ** 2 + (kT * B2) ** 2);
  const betaNum = Math.sqrt((kBeta * B3) ** 2 + (kT * B1) ** 2);

  let alpha = Math.atan2(alphaNum, B1);
  let beta = Math.atan2(betaNum, B2);

  return {
    alpha: scaleAngleToBits(alpha),
    beta: scaleAngleToBits(beta),
  };
}
