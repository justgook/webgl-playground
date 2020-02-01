const gamepads = new Map();

function gamepadHandler(e, connecting) {
    gamepads.set(e.gamepad.index, new GamepadToKeyboard(e.gamepad));
    if (connecting && !gamepads.length) {
        requestAnimationFrame(gamepadTick);
    } else {
        gamepads.delete(e.gamepad.index);
        if (!gamepads.size) {
            cancelAnimationFrame(gamepadTick);
            setTimeout(() => cancelAnimationFrame(gamepadTick), 0);
        }
    }
}


window.addEventListener("gamepadconnected", function (e) {
    gamepadHandler(e, true);
}, false);
window.addEventListener("gamepaddisconnected", function (e) {
    gamepadHandler(e, false);
}, false);

class GamepadToKeyboard {
    timestamp = 0;
    up = false;
    right = false;
    down = false;
    left = false;

    constructor(aaa) {
        this.gamepad = aaa;
    }

    validate(gamepad) {
        if (this.gamepad.timestamp !== gamepad.timestamp) {
            const key = {
                [true]: 'keydown',
                [false]: 'keyup',
            };

            if (this.gamepad.buttons[0] !== gamepad.buttons[0]) {
                document.dispatchEvent(new KeyboardEvent(key[gamepad.buttons[0].pressed], { 'key': ' ' }));
            }
            if (this.gamepad.buttons[1] !== gamepad.buttons[1]) {
                document.dispatchEvent(new KeyboardEvent(key[gamepad.buttons[1].pressed], { 'key': 'Shift' }));
            }
            if (this.gamepad.axes[0] !== gamepad.axes[0]) {
                document.dispatchEvent(new KeyboardEvent(key[gamepad.axes[0] < -0.1], { 'key': 'ArrowLeft' }));
                document.dispatchEvent(new KeyboardEvent(key[gamepad.axes[0] > 0.1], { 'key': 'ArrowRight' }));
            }
            if (this.gamepad.axes[1] !== gamepad.axes[1]) {
                document.dispatchEvent(new KeyboardEvent(key[gamepad.axes[1] < -0.1], { 'key': 'ArrowUp' }));
                document.dispatchEvent(new KeyboardEvent(key[gamepad.axes[1] > 0.1], { 'key': 'ArrowDown' }));
            }

            // console.log(gamepad.axes[9]);
            this.gamepad = gamepad;
        }
    }
}

function gamepadTick() {
    const pool = navigator.getGamepads();
    gamepads.forEach((a, k) => a.validate(pool[k]));
    if (gamepads.size) {
        requestAnimationFrame(gamepadTick);
    }
}
