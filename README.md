brew install zeromq
./dependencies.sh
mkdir -p ~/.ipython/kernels/racket/
cp kernel.json ~/.ipython/kernels/racket/

ipython notebook
