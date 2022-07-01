# git-docker

A Git command using docker.
[git-subrepo](https://github.com/ingydotnet/git-subrepo) is also installed.

# Usage

## Use Git command

```Shell
docker run --rm -it -v ${HOME}/.gitconfig:/root/.gitconfig -v $(pwd):/root/workspace -w /root/workspace -u $(id -u):$(id -g) ghcr.io/k-ishigaki/git-docker git status 
```

NOTE:
 * You need run above command ONLY under Git repository's root directory.

## Applications

### For Docker Desktop for Windows

Put a batch file (`git.bat`) to a directory included in PATH.

```Batchfile
@docker run --rm -it -v %USERPROFILE%\.gitconfig:/root/.gitconfig -v %cd%:/root/workspace -w /root/workspace ghcr.io/k-ishigaki/git-docker git %*
```
Then a Git command available.

### For Docker Desktop for Mac

Put a shell script file (`git`) to a directory included in PATH.

```Shell
#!/bin/sh -e
docker run --rm -it -v $HOME/.gitconfig:/root/.gitconfig -v $(pwd):/root/workspace -w /root/workspace ghcr.io/k-ishigaki/git-docker git "$@"
```
Then a Git command available.
