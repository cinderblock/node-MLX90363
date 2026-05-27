# @cinderblock/mlx90363

Node.js API for manipulating and parsing data packets from the [Melexis
MLX90363][datasheet] magnetic-rotary sensor.

This is the host-side companion to the C++ driver at
[`cinderblock/MLX90363`](https://github.com/cinderblock/MLX90363). The driver
runs on a microcontroller and exchanges raw 8-byte SPI packets with the chip;
this library decodes those packets and builds outgoing ones.

[datasheet]: https://www.melexis.com/en/product/mlx90363/triaxis-magnetic-node

## Status

ESM, TypeScript 5+, Node 22+. Tested on Node 22 and 24 in CI.

## Install

```bash
npm install @cinderblock/mlx90363
```

## Usage

```ts
import {
  parseData,
  makePacket,
  OutgoingOpcode,
  Marker,
  type AlphaMessage,
} from '@cinderblock/mlx90363';

// Build an outgoing GET1 packet (request next Alpha-style measurement)
const out = makePacket({ opcode: OutgoingOpcode.GET1 });
// ... send `out` over SPI to the chip ...

// Parse an incoming 8-byte buffer
const msg = parseData(incoming);
if (msg.marker === Marker.Alpha && msg.crc) {
  const angle = (msg as AlphaMessage).alpha; // 14-bit angle
}
```

## API surface

- `CRC(buffer)` — compute the datasheet CRC over the first 7 bytes
- `makePacket({ opcode, marker?, data8?, data16? })` — build an 8-byte packet
- `parseData(buffer)` — decode an 8-byte packet into a tagged union
- `Marker`, `OutgoingOpcode`, `IncomingOpcode`, `DiagnosticStatus`, `ErrorCode`,
  `EECode`, `MapXYZ`, `NamedEEMemoryLocations` — enums
- `Message<M, T>` and the concrete message variants
  (`AlphaMessage`, `AlphaBetaMessage`, `XYZMessage`, plus all the
  `ReceivedOpcodeMessages`)
- `mapXYZ`, `computeAlpha`, `computeAlphaBeta`, `computeInternal`,
  `defaultConstants`, `Constants` — helpers for reproducing the chip's
  internal alpha/beta computation locally
- `EEchallenge` — challenge keys from the datasheet

## Development

```bash
npm ci
npm run build      # tsc -> dist/
npm test           # node --test against compiled dist/
npm run format     # prettier
```

CI runs the build + test matrix on Node 22 and Node 24. Releases are tagged
(`vX.Y.Z`) and published to npm via OIDC trusted publishing (no NPM_TOKEN
secret).
