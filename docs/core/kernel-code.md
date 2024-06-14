# Kernel functions in `elm/core`

This documents lists all kernel functions defined in JavaScript files contained within `src/Elm/Kernel/`.
Each row in the table corresponds to a function, each kernel module (a JavaScript file) will span multiple rows, one for each function defined in that module.

These kernel functions are private to `elm/core` and thus they can only be called by other functions in `elm/core`.
The last column of the table contains a best effort to exhustively list all the places that call each kernel function.

# List of all kernel functions

| Module                    | Function      | Notes         | Used by       |
| ------------------------- | ------------- | ------------- | ------------- |
| `Elm.Kernel.Basics`       | `add`         | [1](###1)     | `Basics.add`  |
|                           | `sub`         | [1](###1)     | `Basics.sub`  |
|                           | `mul`         | [1](###1)     | `Basics.mul`  |
|                           | `fdiv`        | [1](###1)     | `Basics.fdiv` |
|                           | `idiv`        | [1](###1)     | `Basics.idiv` |
|                           | `pow`         | [1](###1)     | `Basics.pow`  |
|                           | `pow`         | todo...       |
|                           | `remainderBy` | todo...       |
|                           | `modBy`       | todo...       |
|                           | `pi`          | todo...       |
|                           | `e`           | todo...       |
|                           | `cos`         | todo...       |
|                           | `sin`         | todo...       |
|                           | `tan`         | todo...       |
|                           | `acos`        | todo...       |
|                           | `asin`        | todo...       |
|                           | `atan`        | todo...       |
|                           | `atan2`       | todo...       |
|                           | `toFloat`     | todo...       |
|                           | `truncate`    | todo...       |
|                           | `isInfinite`  | todo...       |
|                           | `ceiling`     | todo...       |
|                           | `floor`       | todo...       |
|                           | `round`       | todo...       |
|                           | `sqrt`        | todo...       |
|                           | `log`         | todo...       |
|                           | `isNaN`       | todo...       |
|                           | `not`         | todo...       |
|                           | `and`         | todo...       |
|                           | `or`          | todo...       |
|                           | `xor`         | todo...       |
| `Elm.Kernel.Bitwise`      | `and`             | todo...   |
|                           | `or`              | todo...   |
|                           | `xor`             | todo...   |
|                           | `complement`      | todo...   |
|                           | `shiftLeftBy`     | todo...   |
|                           | `shiftRightBy`    | todo...   |
|                           | `shiftRightZfBy`  | todo...   |
| `Elm.Kernel.Char`         | todo...       |
| `Elm.Kernel.Debug`        | todo...       |
| `Elm.Kernel.JsArray`      | todo...       |
| `Elm.Kernel.List`         | todo...       |
| `Elm.Kernel.Platform`     | `worker`              |                       | `Platform.worker`                                                                                                 |
|                           | `initialize`          |                       | `Elm.Kernel.Browser.document`, `Elm.Kernel.Browser.element` in `elm/browser`                                      |
|                           | `registerPreload`     | [Preload](###Preload) | Never used.                                                                                                       |
|                           | `setupEffects`        |                       | `Elm.Kernel.Platform.Initialize`                                                                                  |
|                           | `createManager`       |                       | Compiler generated JavaScript for event managers.                                                                 |                                                                                                                                                   |
|                           | `instantiateManager`  |                       | `Elm.Kernel.Platform.setupEffects`                                                                                |
|                           | `sendToApp`           |                       | `Platform.sendToApp`                                                                                              |
|                           | `sendToSelf`          |                       | `Platform.sendToSelf`                                                                                             |
|                           | `leaf`                |                       | `Elm.Kernel.Platform.outgoingPort`, `Elm.Kernel.incomingPort`, Compiler generated JavaScript for event managers.  |
|                           | `batch`               |                       | `Platform.Cmd.batch`, `Platform.Sub.batch`                                                                        |
|                           | `map`                 |                       | `Platform.Cmd.map`, `Platform.Sub.map`                                                                            |
|                           | `dispatchEffects`     |                       | todo...                                                                                                           |
|                           | `gatherEffects`       |                       | todo...                                                                                                           |
|                           | `toEffect`            |                       | todo...                                                                                                           |
|                           | `insert`              |                       | todo...                                                                                                           |
|                           | `checkPortName`       |                       | todo...                                                                                                           |
|                           | `outgoingPort`        |                       | todo...                                                                                                           |
|                           | `outgoingPortMap`     |                       | todo...                                                                                                           |
|                           | `setupOutgoingPort`   |                       | todo...                                                                                                           |
|                           | `incomingPort`        |                       | todo...                                                                                                           |
|                           | `incomingPortMap`     |                       | todo...                                                                                                           |
|                           | `setupIncomingPort`   |                       | todo...                                                                                                           |
|                           | `export`              |                       | todo...                                                                                                           |
|                           | `mergeExports`        |                       | todo...                                                                                                           |
| `Elm.Kernel.Process`      | todo...       |
| `Elm.Kernel.Scheduler`    | todo...       |
| `Elm.Kernel.String`       | todo...       |
| `Elm.Kernel.Utils`        | todo...       |

## List of all global mutable state

| Module                    | Variable name      | Notes                    | Used by               |
| ------------------------- | ------------------ | ------------------------ | --------------------- |
| `Elm.Kernel.Platform`     | `preload`          | [Preload](###Preload)    | Never used.           |
| `Elm.Kernel.Platform`     | `effectManagers`   | [Effects](###Effects)    | todo...               |

## Notes

### 1

For elm code like `x + y` or `time / 60`, the official compiler generates javascript using the operators directly.
These functions will only be called if the user does `add x y` or `fdiv time 60`.
I (Harry Sarson) belive that `Basics.add` calls `Elm.Kernel.Basics.add` entirely so that the type resolution part of the compiler can treat `(+)` as an operator defined by `infix left 6 (+) = add`.
The code generation part of the compiler then special cases these operators.

### Preload

Looks like something that was introduced during the exploration of server side rendering.

### Effects

Todo...
