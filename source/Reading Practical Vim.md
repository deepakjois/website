---
title: Notes from reading Practical Vim
published: 2012-05-02
lead: Useful Vim tips from Drew Neil’s book <a href="https://pragprog.com/book/dnvim/practical-vim">Practical&nbsp;Vim</a>
---
### Chapter 1 : The Vim Way

* The Dot Formula
* One keystroke to move, one to execute

### Chapter 2 : Normal Mode

* `daw` is more composable than `bdw` and `dbx`
* `<C-a>` and `<C-x>` can be used to do simple arithmetic
* Operator + Motion = Action : Page 25 for all operators.


### Chapter 3 : Insert Mode

* `C-h`, `C-w` and `C-u` for deleting char, word and line
* `C-o` is Insert Normal mode where u can fire a single command
* `<C-o>zz` allows you to center the line on the screen visually
* We can press `<C-r>0` in insert mode to paste the text that we just yanked at the current cursor position
* `<C-r>=` allows you to perform simple calculations

### Chapter 4 : Visual Mode

* `V` for linewise visual mode
* `o` for changing the free end of the cursor
* Not directly related, but `gU{motoion}` uppercases the `{motion}` text.
* `Vr{char}` is a good way to select a line and replace all characters with `{char}`
* `I` and `A` are your friends when editing ragged visual blocks

### Chapter 5 : Commandline mode

* `:3d` is a an Ex command that is faster than its normal mode equivalent `3Gdd`
* Some ways to specify ranges : `:{start},{end}`, `%` (stands for entire file), `.` (current line)
* `:'<,'>` is pre-populated when we press the `:` key after a visual selection.
* The `'<` symbol is a mark standing for the first line of the visual selection, while `'>` stands for the last line of the visual selection
* Vim also accepts a pattern as an address for an Ex command. For example: `:/<html>/,/<\/html>/p`
* General form of the offset: `:{address}+n`, for e.g. `:/<html>/+1,/<\/html>/-1p`
* Ranges summarised on page 60
* General syntax of copy is `:[range]t {address}`
* General syntax of move is `:[range]m {address}`
* At Vim’s command line, the `<C-r><C-w>` mapping copies the word under the cursor and inserts it at the command-line prompt. Useful for find operations.

### Chapter 6 : Manage multiple files

* `[b` and `]b` from the vim-unimpaired plugin can be smoother to type for
  switching between buffers than `bn` and `bp`.

* `<C-^>` is a great way to navigate to last used buffer.

### Chapter 7 : Open files and save them to disk

* `:edit %<Tab>` and `:edit %:h<Tab>` expand out the filepath for the active buffer.
* `:E` is shorthand for `:Explore`

### Chapter 8 : Get around faster

* An interesting motion command hack: Instead of searching for the first
  character of the word and using `dw`, search for a character in the middle of
  the word and use `daw`. This is useful if the first character of the word is
  too common, but there are other characters that are uncommon. For e.g. the word
  _excellent_, where `e` is common, but `x` is uncommon.
* The search command is an _exclusive_ motion. This means that searching for a
  string will place the cursor _before_ the string if it matches. This is useful
  in combining the `d{motion}` operator with the search motion, to delete
  portions of text.
* `vi{` : Select text inside the `{}` block.
* `mm` and `m\`` to mark something and snap back to it.
* Upper case letters create global marks. You could use them to jump between files.

### Chapter 9 : Navigate between files with jumps

* In web browsers, we’re used to using the back button to return to pages that
  we visited earlier. Vim provides a similar feature by way of the jump list:
the `<C-o>` command is like the back button, while the complementary `<C-i>`
com- mand is like the forward button.
* Table of jumps. Refer to it if you need to find the different kind of jumps.
* Using the `g;` and `g,` commands, we can traverse backwards and forwards
  through the changelist (`:h changelist`). As a memory aid for `g;` and `g,`,
it may help to remember that the `;` and `,` commands can be used to repeat or
reverse the `f{char}` command.
* `\`.` marks the position of last change.
* `\`^` marks the positions of last change.
* We need somehow to instruct Vim to modify the filepath under the cursor by
  appending the .rb file extension before attempting to open it. We can do
  this with the `suffixesadd` option.

### Chapter 10 : Copy and paste

* Cut, Copy and Paste => Delete, Yank and Put
* Taken together, the `xp` commands can be considered as: ‘transpose the next
  two characters.’
* The `ddp` sequence could be considered to stand for: transpose the order of
  this line and its successor.
* `yyp` can be used to duplicate a line
* It can be very useful to understand the difference between `p` and `P` in
  actual practise. `p` puts *after* the cursor and `P` *in front of* the
cursor.
* The delete, yank, and put commands all interact with one of Vim’s registers.
  We can specify which register we want to use by prefixing the command with
`"{register}`. If we don’t specify a register, then Vim will use the unnamed
register.
* `"%` register contains name of the current file. Information about other such
  read only registers can be found in `:h quote`.
* `"+` refers to the system clipboard.
* When we use the `p` command in Visual mode, Vim replaces the selection with the
  contents of the specified register.
* Swap two words example on page 129 is a good example of using marks. I like
  the technique of using `mm` to use `m` as the mark register to save some
  typing.
* `gp` and `gP` leave the cursor after the new text.
* `:set paste` is the best solution to all those times that I had problems with
  pasting text in Vim when using it in terminal. It can be made even easier by
  using something like `set pastetoggle=<F5>`.

### Chapter 11 : Macros

**TODO** : Read the section about editing macros again.

* Inspect the contents of a register (for example `a`) by typing `:reg a`
* The @{register} command executes the contents of the specified register.
* The :argdo command allows us to execute an Ex command once for each buffer in the argument list.

### Chapter 12 : Matching Patterns and Literals

**TODO** Revisit this chapter once you have had more experience searching.

* `\v` for `nomagic` search
* `\V` for verbatim search
* In a very magic search, the naked < and > characters are interpreted as word delimeters. For e.g, `/\v<the>` matches `the` but not `their`.

### Chapter 13 : Search

* `:set nohlsearch` disables highlighting entirely. `:nohlsearch` can be used
  to mute highlighting temporarily.
* Search offset feature. `/<search keyword>/e` places the cursor at the end of
  the match when we navigate to it using `N` or `n`.
* `:%s//` will execute a substitution in the context of the previous search. So
  we can provide it with a substitution string, captures etc.

### Chapter 14 : Substitution

* Substitute command : `:[range]s[ubstitute]/{pattern}/{string}/[flags]`
* `~` in the replacement field, is a special character which denotes the string
  from previous invocatin of substitute
* Leaving the search field of the substitute command blank instructs Vim to
  reuse the most recent search pattern. This tech- nique allows us to decouple
  two tasks: composing a (complex) pattern, and devising a suitable replacement
  string
* Suppose that we’ve yanked a multiline selection of text, and it’s stored in
  register 0. Now we want to use that text as the replacement field of the
  substitute command. We could do so by running this: `:%s//\=@0/g`
* `/\v(<man>|<dog>)` and `:%s//\={"dog":"man","man":"dog"}[submatch(1)]/g` lead
  to a substitution where `dog` is replaced by `man`, and `man` is replaced by
  `dog`

### Chapter 15 : Global commands

* `:g//d` deletes all lines in buffer matching last line.
* `:v/href/d` does the opposite of `g` above. It deletes all lines that do no
  match the pattern specified.
* `print` is the default command that is used by `:global` if no command is
  specified. For example, `:g/TODO` will just print all the todo items in the
  file.
* `:.+1,/}/-1 sort` - from the next line (`.+1`) to the line preceding the line
  that matches a closed curly brace (`/}/-1`)

### Chapter 16 : Index and navigate source code with ctags

* On OS X do : `brew install ctags`, on Linux : `sudo apt-get install exuberant-ctags`
* Effortless ctags with git : http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html
* Instead of `<C-]>`, we could use the `g<C-]>` command. Both of these commands
  behave identically in the case when the current keyword has only a single match.
  But if it has multiple matches, then the `g<C-]>` command presents us with a list
  of choices from the tag matchlist.
* Nice table on Page 215 listing all commands related to tags.

### Chapter 17 : Compile code and navigate errors with the quickfix list

* Set a custom compiler using `:setlocal makeprg=NODE_DISABLE_COLORS=1\
  nodelint\ %`
* Here is a sample `errorformat` string to parse nodejs output:
  `:setlocal efm=%A%f\,\ line\ %l\,\ character\ %c:%m,%Z%.%#,%-G%.%#`
* We could also setup a shortcut like `:compiler nodelint`, if we have the
  following in `quickfix/ftplugin.javascript.vim`

```
setlocal makeprg=NODE_DISABLE_COLORS=1\ nodelint\ %
   let &l:efm='%A'
   let &l:efm.='%f\, '
   let &l:efm.='line %l\, '
   let &l:efm.='character %c:'
   let &l:efm.='%m' . ','
   let &l:efm.='%Z%.%#' . ','
   let &l:efm.='%-G%.%#'
```

* `:args $VIMRUNTIME/compiler/*.vim` to familiarize yourself with the plugins
  that are distributed with Vim by running the command.

### Chapter 18 : Search projectwide with grep, vimgrep, and others

* Install ack-vim: https://github.com/mileszs/ack.vim
* `vimgrep` integrates nicely with Vim's inbuilt search

### Chapter 19 : Dial ‘X’ for auto-completion

* Page 277 has all the shortcuts related to auto-completion
* `<C-e>` reverts to the originally typed text (exit from auto-completion)
* `<C-x><C-l>` triggers linewise auto-completion which might be useful when coding.

### Chapter 20 : Find and fix typos with Vim’s spell checker

* `:set spell` sets spell checker mode
* We can jump backwards and forwards between flagged words with the `[s` and
  `]s` commands, respectively, and use `z=` to suggest a list of corrections
* Nice summary on page 249
* We could fix the error from Insert mode using the `<C-x>s` com- mand, which
  triggers a special form of auto-completion

### Appendix 1 : Customizing Vim to suit your preferences

Here is a way to customize the indentation for different file types:

```
if has("autocmd")
        filetype on
        autocmd FileType ruby setlocal ts=2 sts=2 sw=2 et
        autocmd FileType javascript setlocal ts=4 sts=4 sw=4 noet
endif
```
## Notes from talk on YouTube: Let Vim Do the Typing

Talk link: <https://www.youtube.com/watch?v=3TX3kV3TICU>

* `<C-r>` in insert mode to paste contents of a register
* `<C-a>` in insert mode to paste last inserted text
* `<C-p>` and `<C-n>` in insert mode are Vim’s default keybindings for completion
* `<C-x>` is a special submode of completion, which could be used for example to complete only tags. `<C-x> <C-]>`
* `<C-x> <C-f>` for filename completion
* `<C-x> <C-p>` and  `<C-x> <C-n>` for context-aware completion.
* `<C-x> <C-l>` for context-aware line completion


