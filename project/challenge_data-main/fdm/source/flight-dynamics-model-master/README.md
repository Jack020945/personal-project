# ATHENS UAV Flight Dynamics Model

This tool calculates flight dynamics model properties for a propeller based on
the theory from the books "Aircraft Control and Simulation" 3rd edition, by
Stevens, Lewis, and Johnson, 2016, (some code on page 182-183), and from
"Introduction to Helicopter and Tiltrotor Flight Simulation" 2nd edition, by
Dreier. Modified by Walker.

[[_TOC_]]

## Build

The tool is written in FORTRAN, and can be compiled using the GNU Autotools
chain.

### Build for POSIX-like or \*nix Operating System

#### Prepare

Most POSIX-like operating systems already include the necessary build tools.
If the following commands fail, check that the \*nix system has the necessary
tools installed.  For example, on an Ubuntu Linux system, running the following
command should suffice:

```shell
sudo apt install build-essential libtool-bin gfortran
```

#### Build

Once the necessary tools are in place, to build on a POSIX-style operating
system:

1. `cd` into the project directory
1. run:
    ```shell
    autoreconf -if
    ./configure
    make
    ```

The resulting tool is called `new_fdm` and is located in the `bin`
subdirectory.

#### Install

It is also possible to install the `new_fdm` binary so that it is available in
the path of a \*nix system.

To install the binary, run (as a superuser):

```shell
make install
```

or (as a non-superuser):

```shell
sudo make install
```

#### Uninstall

To uninstall the binary, run (as a superuser):

```shell
make uninstall
```

or (as a non-superuser):

```shell
sudo make uninstall
```

### Build for Windows, on Windows (**Currently Broken**)

**NOTE:** Currently, building within `MSYS` on Windows is broken when using
`libtool`.  If you know how to fix this, please feel free to submit a merge
request.  Regardless, it is still possible to cross-build for Windows from a
*nix operating system.  See next [major section](#build-for-windows-using-a-posix-like-or-nix-operating-system).

To build for Windows and on Windows, some dependencies must be installed.

#### Prepare

To build for Windows on Windows, install the following package:

* [`MSYS2`](https://www.msys2.org/)

Follow the instructions at that link to get a build environment setup.  This
includes the following post-install steps:

1. Run `MSYS2 64-bit` from the Start menu:
  1. run `pacman -Syu`
  1. close the Window when prompted
1. Run `MSYS2 MSYS` from the Start menu:
  1. run `pacman -Su`
  1. run `pacman -S --needed base-devel mingw-w64-x86_64-toolchain`
  1. close the Window (type `exit` or `Ctrl-D`)
1. To use `make` or `gfortran`, run `MSYS2 64-bit` from the Start menu

#### Build

After setting up `MSYS2`, building on Windows involves the same steps as Linux.

1. Run `MSYS2 64-bit` from the Start menu
1. `cd` to the project directory
1. run:
    ```shell
    autoreconf -if
    ./configure
    make
    ```

The resulting tool is called `new_fdm.exe` and is located in the `bin`
subdirectory.  From an `MSYS` console, this executable can be run as on a
POSIX-like OS.  To run it from a Windows `CMD.exe` shell or `PowerShell`, some
dependencies need to be copied to the same directory as the executable.

#### Copy `DLL`s

In order to make the `new_fdm.exe` Windows executable portable, and runnable
from a Windows `CMD.exe` shell or `PowerShell` console, a few dependencies need
to be copied into the same directory as the `new_fdm.exe` executable.  This
task can be automated via the use of the `copy_dlls_win.sh` script.

To copy the necessary `DLL` files to the `bin` directory:

1. Run `MSYS2 64-bit` from the Start menu
1. `cd` to the project directory
1. run `./copy_dlls_win.sh`

After this, as long as those `DLL` files are in the same directory as the
executable file, it can be run from a normal Windows command shell.


#### Install and Uninstall

The Windows executable `new_fdm.exe` can be installed and uninstalled in
Windows in the same manner as in a \*nix system, which will make it accessible
in the path of an `MSYS2` shell, but will not necessarily make it accessible from
a normal Windows `CMD.exe` shell or `PowerShell` console.

To do so, follow the install steps for POSIX-like and \*nix systems.


### Build for Windows using a POSIX-like or \*nix Operating System

The MinGW project supports cross-building, and so it is possible to build a
Windows executable on a POSIX-like OS.

#### Prepare

Setting up MinGW for any specific POSIX-like OS takes different steps.  For
most cases, instructions can be found at [the official `mingw-w64`
website](http://mingw-w64.org/doku.php).

For Ubuntu Linux, for example, the following command installs the necessary
tools:

```shell
sudo apt install build-essential mingw-w64 gfortran-mingw-w64 libtool-bin
```

#### Build

Once the necessary tools are in place, to build for Windows on a POSIX-style
operating system:

1. `cd` into the project directory
1. run:
    ```shell
    autoreconf -if
    ./configure --host=x86_64-w64-mingw32 --prefix=${PWD}/dist
    make
    make install
    ```

The resulting tool is called `new_fdm.exe` and is located in the `dist/bin`
subdirectory.

Similarly to when building for Windows on Windows, the `DLL` dependencies will
need to be copied into the same directory as the executable.

#### Copy `DLL`s

In order to make the `new_fdm.exe` Windows executable portable, and runnable
from a Windows `CMD.exe` shell or `PowerShell` console, a few dependencies need
to be copied into the same directory as the `new_fdm.exe` executable.  This
task can be automated via the use of the `copy_dlls.sh` script.

**NOTE:**  This presumes you used `${PWD}/dist` as the argument to `--prefix`
when running `./configure` above.

To copy the necessary `DLL` files to the `dist/bin` directory:

1. `cd` to the project directory
1. run `./copy_dlls.sh`

After this, as long as those `DLL` files are in the same directory as the
executable file, it can be run from a normal Windows command shell.


## Run

### \*nix

To run `new_fdm` on a \*nix system:

1. `cd` to the project directory
1. Run the following:
    ```shell
    cd bin
    ./new_fdm < input.inp > output.out
    ```

The `input.inp` [^1] file represents the model details for `new_fdm` to process.

The `output.out` file is an ASCII text file containing the results.

### Windows

To run `new_fdm.exe` on a Windows system:

1. Open a shell (`CMD.exe`, `PowerShell`, or, if installed, `MSYS2`):
1. `cd` to the project directory
1. Run the following:
    ```shell
    cd bin
    ./new_fdm.exe < input.inp > output.out
    ```

The `input.inp` [^1] file represents the model details for `new_fdm` to process.

The `output.out` file is an ASCII text file containing the results.

### Docker

It is possible to run `new_fdm` directly from a Docker container.

#### Initial Setup

1. Install Docker.
    * Instructions for this are out of scope for this document, but readily
      available online.
1. Log into the Vanderbilt ISIS Gitlab Docker Repository.
    * Open a console or terminal.
    * Use the following command:
        ```shell
        docker login git.isis.vanderbilt.edu:5050
        ```
    * You will use your Vanderbilt ISIS Gitlab username and password.
    * Your credentials should be cached, and so this should only be necessary
      one time.

#### Run

To run `new_fdm` via Docker use the following command [^2]:
* Linux `BASH` shell (or equivalent):
    ```shell
    docker run -i \
        -v ${PWD}:/app/run \
        --rm \
        git.isis.vanderbilt.edu:5050/swri/flight-dynamics-model:master \
        < input.inp \
        > output.out
    ```
* Windows `CMD.exe`:
    ```shell
    docker run -i \
        -v %CD%:/app/run \
        --rm \
        git.isis.vanderbilt.edu:5050/swri/flight-dynamics-model:master \
        < input.inp \
        > output.out
    ```
* Windows `PowerShell`:
    ```powershell
    docker run -i \
        -v ${PWD}:/app/run \
        --rm \
        git.isis.vanderbilt.edu:5050/swri/flight-dynamics-model:master \
        < input.inp \
        > output.out
    ```

The `input.inp` [^1] [^3] file represents the model details for `new_fdm` to process.

The `output.out` file is an ASCII text file containing the results.

## Notes

[^1]: The input file (here `input.inp`) is mostly self-contained, but does
      reference propeller data files by their filesystem path.  You must ensure
      that the paths to those files are correct for `new_fdm` to run correctly.

[^2]: The only difference between the three approaches is the `-v xxx:/app/run`
      volume mount.  This volume mount is necessary because `new_fdm`, when
      executed, writes output to `stdout` *and also places output files in the
      runtime directory*.  Inside the Docker container, the runtime directory is
      `/app/run`.  By mounting the current directory to `/app/run`, we ensure
      the output files are written to the current directory, which is the
      expected behavior when running `new_fdm` outside of Docker.  Technically,
      any directory can be used to capture these outputs by replacing the `xxx`
      in the example above with the directory to place the output.

[^3]: For the Docker version of `new_fdm`, the propeller files are located at
      `/app/Tables/PropData`, which, relative to the runtime directory, is at
      `../Tables/Propdata`.  This is the same location, relative to
      `new_few.exe`, that the propeller data files are located in the ATHENS
      pipeline, so input files already in use should not need to be changed.
      Also note that it is possible to mount an external volume at `/app/Tables`
      inside of the Docker container, so a different set of propeller data files
      can be used if desired.