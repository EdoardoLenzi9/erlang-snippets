# Erlang snippets

Test repo to get started with erlang following [user reference manual](http://erlang.org/doc/reference_manual/users_guide.html) examples.


## Bootstrap

Install the latest repo using you package manager (not always up to date with the latest version)

```
sudo apt-get install -y erlang
```

Or using [kerl](https://github.com/kerl/kerl) (up to date with the latest version)


```
kerl list releases
> ...22.0-rc3

kerl build 22.0-rc3 build-22.0-rc3
```

> [missing dependencies](https://github.com/asdf-vm/asdf-erlang/issues/83)

```
kerl list builds
> 22.0-rc3,build-22.0-rc3

kerl install build-22.0-rc3 [path]
kerl list installations
> build-22.0-rc3 [path]

# ~.bash_aliases
alias kerl_activate='. [paht]/activate'
alias kerl_activate='. /home/eddy/kerl/22.0/activate'

kerl_deactivate
```


### IDE

vscode extensions or [erlide](https://erlide.org/)

