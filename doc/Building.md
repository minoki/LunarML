
# Building

*How to compile this project.*

<br>

## Requirements

- Recent version of **[MLton]**

- **[Lua 5.3+]** or a recent **[NodeJS]** version

<br>
<br>

## Steps

```shell
make
make test
make test-stackless-handle
make test-nodejs
make test-nodejs-cps
```

```shell
./lunarml example/hello.sml
```

```console
user@system:~$ lua example/hello.lua
Hello world!
```

<br>


<!----------------------------------------------------------------------------->

[Lua 5.3+]: http://www.lua.org/
[NodeJS]: https://nodejs.org/en/
[MLton]: http://mlton.org/
