# Intro

**proteome** is a [Neovim] plugin written in [Haskell] and powered by [ribosome] and [nvim-hs].

It provides project-specific configuration file loading and performs runtime tasks on projects.

# Install

There are two alternative methods for integrating this plugin into your Neovim.
In both cases, the plugin will bootstrap your machine with [stack] and install itself on startup, as well as rebuild
when the repository is updated.

**Note** that at the moment, most of the plugin's functionality is dependent on using the **chromatin** installation method.

## nvim-hs

The basic variant is to use the built-in package management facility of the rplugin provider.
In this case, you have to include [nvim-hs.vim]:

```vim
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'tek/proteome'
```

## chromatin

The author maintains a manager for [ribosome]-based plugins called [chromatin] that has additional features.
You will be presented with a terminal buffer containing the output of the installation routines whenever something is
built.

```vim
Plug 'tek/chromatin'
Plug 'tek/proteome'
```

# Overview

The principle of this plugin is that when you run Neovim, the current directory
may host a project with a type and name, and that you want to set up
configuration specific to this project.

The plugin figures out the type and name on startup and, if that was
successful, proceeds to load configuration from several files in
`{runtimepath}/project`.

Additional projects can be added at runtime; you can then switch the working
directory between them.

## Configuration

The most important configuration is read from the variable
`g:proteome_project_config` with the schema:

```vim
{
  'baseDirs': ['directories'],
  'typeDirs': ['directories'],
  'projectTypes': { 'type': 'directories' },
  'typeMap': { 'type': ['types'] },
  'typeMarkers': { 'type': ['filenames'] },
  'langMap': { 'type': 'lang' },
  'langsMap': { 'lang': ['langs'] },
}
```

All parts are optional, but you probably want at least one of `baseDirs`,
`typeDirs` and `typeMarkers`.

# Project Detection

**proteome** needs to determine the name and type of your project.
The type can be arbitrarily defined for your own purposes, but mostly you
should want it to represent the project's main language.
The builtin tools can detect the type from the contents or the path of the
project.

## Project Content

The project config variable's key `typeMarkers` is read as a mapping of project
type names to filename globs that are matched against the working directory.
If any of the globs match, the corresponding type will be used for the project.
A few defaults are built-in, so if you're lucky you won't have to configure
anything.

## Project Path

Path based type detection uses the project config keys `baseDirs`, `typeDirs`
and `projectTypes`.
The most direct variant is `projectTypes`, which simply maps types to a list of
concrete paths.
If the working directory is among the paths, the corresponding type will be
chosen.

The `baseDirs` variant assumes that your projects are arranged as `type/name`
subdirectories of a common base directory.
If the working directory matches `basedir/type/name`, where `basedir` is one of
the directories in this config option, the path segments will be used for type
and name.

The `typeDirs` variable is similar to that, it assumes `basedir/name`, with
`basedir` taken from the values and the type from the keys in the config.

## Additional Types and Languages

The `typeMap`, `langMap` and `langsMap` settings can be used to add types and
languages to a project based on the main type.

# Project Specific Config

When **chromatin** triggers plugin initialization, **proteome** loads the
following sequence of files from the `project` subdirectory in your
`runtimepath`:

* `all.vim`
* `type.vim`
* `type/name.vim`

In the first stage of initialization, the `project` directory is used.
After that, the second stage loads the same set of files from the
`project_after` directory.

# `ProSave`

This command is intended to be used when saving files and executes the tag
generation as well as writing the current list of buffers to disk.

An autocmd will execute it for you whenever you save a file, waiting a number
of seconds (`g:proteome_save_interval`) between executions to avoid running
multiple times when executing `:wa`.

# Tags

The command `ProTags` triggers the execution of `ctags` or another tag
generation tool.
It can be configured with these variables:

* `g:proteome_tags_command` The executable, like `ctags`.
* `g:proteome_tags_args` A template string for the arguments passed to the command.
* `g:proteome_tags_file_name` The final name for the tags file, default `.tags`.
* `g:proteome_tags_fork` Whether to run in a thread.

To prevent the tag file from being gone while the tagger is running, the
process outputs first to a temporary file and renames it if it was successful.

The args template can contain several variable that will be filled in before
executing the command:

* `{langsComma}` A comma-separated list of project languages (default is the project type).
* `{tagFile}` The temporary file name that the command should write to.
* `{root}` The project root directory, useful when having multiple projects.

The default template, for `ctags`, is `-R --languages={langsComma} -f {tagFile} {root}`.

# Grep

The command `ProGrep` will execute `grep` or another, configurable, program
using the specified pattern and display the results in a menu.

Mappings:
* `<space>` to mark an item
* `<cr>` to jump to a match
* `y` to yank the matching line
* `r` to start replace mode
* `d` to delete the matching line

Deleting a line will compensate for resulting double empty lines.

`ProGrepIn(path, pattern)` allows you to specify the directory to search.

`ProGrepOpt(options, pattern)` allows you to specify additional options for the grep program.
For command invocation, whitespace must be escaped:

```vim
ProGrepOpt -r\ bar foo
```

`ProGrepOptIn(path, options, pattern)` combines the last two.

The function `ProGrepList(path, options, pattern)` returns a list of dictionaries with the results.

## Configuration

`g:proteome_grep_cmdline` is read as a template string that configures the
program and options to be executed.
The strings `{pattern}` and `{path}` will be replaced accordingly.
If those strings are missing, they will be appended.

Default:

```vim
let g:proteome_grep_cmdline = 'grep -Hnr {pattern} {path}'
```

The program should be configured to output vim-compatible results.
Example for `rg`:

```vim
let g:proteome_grep_cmdline = 'rg --vimgrep --no-heading --multiline'
```

## Replace Mode

When starting replace mode, all marked lines are loaded into a scratch buffer.
When this buffer is saved, the modified lines will be written to the
corresponding files.

If a line is left empty, it will be deleted, along with the line taking its
place, if both surrounding lines are either whitespace or the buffer edge.

# Buffers

The command `ProBuffers` displays the currently loaded file buffers in MRU
order.

Mappings:
* `<space>` to mark an item
* `<cr>` to load a buffer
* `d` to delete a buffer
* `D` to delete a buffer, discarding changes
* `w` to wipe a buffer
* `W` to wipe a buffer, discarding changes

Config:
* `g:proteome_buffers_current_last` Boolean indicating whether to display the
  active buffer at the end of the MRU list

# Files

The command `ProFiles` takes a list of directories and lists all files inside,
recursively.
Selecting multiple files is supported.
The default is to use `rg`, with fallback on a builtin directory traversal algorithm.

Mappings:
* `<space>` to mark an item
* `<cr>` edit a file
* `<tab>` complete the longest common prefix of existing directories
* `<c-y>` edit a new file with the path given on the prompt, creating necessary directories

Config:
* `g:proteome_files_exclude_hidden` Boolean indicating whether to exclude hidden files
* `g:proteome_files_exclude_files` List of regexes to use as exclude filter for files
* `g:proteome_files_exclude_directories` List of regexes to use as exclude filter for directories
* `g:proteome_files_exclude_wildignore` Boolean indicating whether to honor the nvim option `'wildignore'`
* `g:proteome_files_use_rg` Set to `v:false` to prevent the file name collection from using `rg`

The regexes are matches against the entire path.

# File Paths

The commands `ProMove` and `ProCopy` move or copy the current buffer's file to
the specified location, considering whether the destination is a directory.

If the argument does not contain any slashes, the file is renamed, keeping its
containing directory.
If you want to move something to the top level directory, prefix the argument
with `./`.

If the old file name had an extension and the new one doesn't, the old
extension is added.

The command `ProRemove` moves the current file to a trash dir, located in
`$XDG_CACHE_DIR/proteome/trash`.

Convenient aliases could be:

```vim
command! -nargs=+ -complete=file Mv ProMove <args>
command! -nargs=+ -complete=file Cp ProCopy <args>
command! Rm ProRemove
```

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[ribosome]: https://github.com/tek/ribosome
[chromatin]: https://github.com/tek/chromatin
[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[nvim-hs.vim]: https://github.com/neovimhaskell/nvim-hs.vim
[stack]: https://docs.haskellstack.org/en/stable/README
