# Vi-Debugger

Wrapper for GDB to run `.vi` files.

If not autodetected, add a new debug configuration in `.vscode/launch.json` and use the predefined **Vi Debugger** config (type `Vi` inside **configuration** and the snippet should appear).

When a debug session starts, this extension adds debug tasks to `.vscode/tasks.json`.

If the tasks are not added successfully, you can manually add them using the command:
**Vi Debugger: Ensure Tasks**.

**Note:** Comments in `tasks.json` are not supported.

Don't forget to specify `viDebugger.compilerPath` and `cwd` (if needed) in `launch.json`.
