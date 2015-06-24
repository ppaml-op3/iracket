#!/bin/bash

# echo commands as they're executed
set -x

# exit if non-zero exit code
set -e


IPY_LOC=$(ipython locate)

IRACKET_SRC_DIR=$(pwd)

RACKET_KERNEL_DIR=${IPY_LOC}/kernels/racket

mkdir -p ${RACKET_KERNEL_DIR}

sed "s#IRACKET_SRC_DIR#$IRACKET_SRC_DIR#g" < static/kernel.json > ${RACKET_KERNEL_DIR}/kernel.json

echo "Kernel json file copied to $RACKET_KERNEL_DIR/kernel.json"


echo -n Download and Install C3? [Y/n]
read ANS
# "y", "Y" or just enter means yes. everything else means no
case "z$ANS" in
    z[yY][eE][sS]|z[Yy]|z)
        # "Yes", "y" or just enter means yes.
        IPYTHON_PROFILE_STATIC=$(ipython locate profile ${IPYTHON_PROFILE})/static
        cp ./static/ic3.js ${IPY_LOC}/nbextensions/ic3.js
        cat ./static/custom.js >> $IPYTHON_PROFILE_STATIC/custom/custom.js
        curl -L https://github.com/mbostock/d3/raw/v3.5.5/d3.min.js > $IPYTHON_PROFILE_STATIC/d3.js
        curl -L https://github.com/masayuki0812/c3/raw/0.4.10/c3.min.js > $IPYTHON_PROFILE_STATIC/c3.js
        curl -L https://github.com/masayuki0812/c3/raw/0.4.10/c3.min.css > $IPYTHON_PROFILE_STATIC/c3.css
        echo "Installed C3 into $IPYTHON_PROFILE_STATIC and customized $IPYTHON_PROFILE_STATIC/custom/custom.js"
    ;;
    *)
        # everything else means no
        true
        ;;
esac

echo Installation Succeeded.
