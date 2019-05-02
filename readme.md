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


### Compile and [make](http://erlang.org/doc/man/make.html#all-0) 

```
erl -make

erl
make:all()

alias cerl='/home/eddy/Note/Magistrale/Magistrale/Distributed\ Systems/Project/Examples/erl_compile_all.sh'
```


### Code Path

* [Issues on code path update](https://stackoverflow.com/questions/20570508/whats-the-difference-between-codeadd-path-and-using-pa-on-the-command-line)
* [Code module](http://erlang.org/doc/man/code.html)

```
code:get_path().
code:add_path("folder_path").
code:add_path("./factorial").

erl -pa ./factorial

factorial:fac(3).     
```


### IDE

vscode extensions or [erlide](https://erlide.org/)


### Resources

* [Erlang for beguinners](https://www.tutorialspoint.com/erlang/erlang_modules.htm)


## Docker

```
sudo docker search erlang
sudo docker pull erlang
sudo docker run -it erlang
```