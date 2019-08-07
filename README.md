# Asemple set of dotfiles
Jumping on the [dotfile management train](http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/) this repository is my cobbled together configuration. Which itself is sourced from a few differnet collections of different dotfile repositories which I try to attribute when I remember the source. For other examples, take a look at the [GitHub dotfiles](https://dotfiles.github.io/) site.

## Getting Started
Checkout to the location on your machine where you keep your repositories. In my case that is the `~/src/personal` directory. Then run the `.install.sh` script.

### GNU Stow
The install script uses [GNU Stow](https://www.gnu.org/software/stow/) to manage the files. It works by creating a symlink form the file in this repository to the target directory. In our case, stow is called on each separate top level folder in this repository.

For a great overview of this setup, have a look at [this post](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html).
