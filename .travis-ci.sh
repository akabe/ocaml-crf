# OPAM packages you install
OPAM_DEPS="ocamlfind gsl slap ounit"

export PREFIX="./usr"
export BINDIR="$PREFIX/bin"
export LIBDIR="$PREFIX/lib"
export PATH="$BINDIR:$PATH"

mkdir -p $PREFIX

# Download and install OPAM and OCaml
wget -q -O opam_installer.sh "https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh"
if [ -n "${OPAM_VERSION:-}" ]; then
    sed -i "s/^VERSION=.*$/VERSION='$OPAM_VERSION'/" opam_installer.sh
fi
echo y | sh opam_installer.sh $BINDIR $OCAML_VERSION

export OPAMYES=1
export OPAMVERBOSE=1
opam init
eval `opam config env`

# Install OPAM packages
if [ -n "${OPAM_DEPS:-}" ]; then
    opam install $OPAM_DEPS
fi

# Build and install your program
./configure --prefix=`opam config var prefix` --enable-tests
make
make install

# Test your program
make test
