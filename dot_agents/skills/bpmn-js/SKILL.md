---
name: bpmn-js
description: >-
  Author BPMN 2.0 XML that bpmn-js and https://demo.bpmn.io/ can open.
  Use when the user asks for BPMN, bpmn.js, a .bpmn file, demo.bpmn.io,
  a Camunda-style process map, or a drop-in preview of a current or target journey.
license: personal
compatibility: all
metadata:
  audience: developer
---

# bpmn-js

Write a `.bpmn` file.
Do not write a hosted bpmn-js page unless the user asks for one.

The preview path is: open https://demo.bpmn.io/ and drop the file on the canvas.

## Where the file lives

If a pi-job task owns the work, write the file under that task `references/`.
Otherwise write it under `docs/`.
Give the user the absolute path.

## Model the journey

1. State current vs target in the process name or a text annotation.
   Do not mix shipped behaviour and a proposal on one path without a label.
2. Use one pool per journey, or two start events that join.
3. Name tasks `System: action`.
4. Use exclusive gateways for join, eligibility, and hand-off.
5. Name end events by outcome.

Waits use an intermediate timer catch event.
Do not model a wait as a task.

```xml
<bpmn:intermediateCatchEvent id="Timer_Lab" name="Until lab results">
  <bpmn:incoming>Flow_In</bpmn:incoming>
  <bpmn:outgoing>Flow_Out</bpmn:outgoing>
  <bpmn:timerEventDefinition id="Timer_Lab_def"/>
</bpmn:intermediateCatchEvent>
```

User work stays a task.
A clock hold stays a timer.

## Colour

Colouring everything by system is too much.
Colouring output is a nice idea.

Default paint:

| Element | Stroke | Fill |
|---|---|---|
| Success / prescribe end, and its last flow | `#2E7D32` | `#C8E6C9` |
| Cancel / skip / fail end, and its last flow | `#C62828` | `#FFCDD2` |
| Timer (optional) | `#F9A825` | `#FFF9C4` |

Leave tasks and gateways uncoloured unless the user asks for more.
A pool may take a light tint only when the pool itself is a gate (for example UK non-prod).

Do not add a CSS class, overlay, or custom renderer.
Those do not survive a demo drop.

Persist both vendor attributes on the DI shape or edge:

```xml
xmlns:bioc="http://bpmn.io/schema/bpmn/biocolor/1.0"
xmlns:color="http://www.omg.org/spec/BPMN/non-normative/color/1.0"
```

```xml
<bpmndi:BPMNShape id="End_Ok_di" bpmnElement="End_Ok"
                  bioc:stroke="#2E7D32" bioc:fill="#C8E6C9"
                  color:border-color="#2E7D32" color:background-color="#C8E6C9">
```

On an edge, set stroke and `color:border-color` only.

## XML contract

- Root is `bpmn:definitions` with a `BPMNDiagram` and waypoints.
- Documentation diagrams set `isExecutable="false"`.
- Every node lists `incoming` and `outgoing`.
- Event box: 36x36.
  Gateway: 50x50.
  Task: 160x80.
- Boxes must not overlap.
  Keep nodes inside their pool.

## Validate

Parse with `bpmn-moddle` before you hand the file back.
Install it in a throwaway dir if it is missing: `cd /tmp/bpmn-check && npm install bpmn-moddle`.

```bash
FILE=/abs/path/file.bpmn node --input-type=module <<'JS'
import { BpmnModdle } from 'bpmn-moddle';
import { readFileSync } from 'fs';
const { rootElement } = await new BpmnModdle().fromXML(readFileSync(process.env.FILE, 'utf8'));
if (!rootElement) process.exit(1);
console.log('ok', rootElement.$type);
JS
```

Unknown `bioc` / `color` attributes from a bare parse are fine.
A thrown `fromXML` error is not.

Fix the named error.
Do not rewrite the diagram from scratch.
