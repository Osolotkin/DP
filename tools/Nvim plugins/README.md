# Vi Language Support for Vim

Provides syntax highlighting and filetype detection for `.vi` files in Neovim.

# Installation

If your plugin manager supports local plugins, add the plugin directory accordingly.

**Example using `lazy.nvim` in Neovim:**

```lua
{ dir = '/lang/tools/Nvim plugins/language-supprot' }
```

Alternatively, manually copy the plugin files to the appropriate directories inside `$VIMRUNTIME` (In case of Vim).

To locate your `$VIMRUNTIME` directory, run the following command inside Vim or Neovim:

```vim
:echo $VIMRUNTIME
```