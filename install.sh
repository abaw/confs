#!/bin/sh

die ()
{
    echo $*
    exit 1
}

readlink_r ()
{
    test -z "$1" && die "readlink_r need an argument"
    path="$1"
    while test -h $path;
    do
	tmp=$(readlink -e $path) || { echo $path; return 1; }
	path=$tmp
    done
    echo $path
}

realpath ()
{
    readlink -f "$1"
}

which readlink>/dev/null || die need "readlink" program to run

machine=$1
test -z $machine && die "Usage: $0 <machine>"
top_dir=$(dirname $0)
machine_dir=$(realpath $top_dir/machines/$machine)
test -d $machine_dir || die "invalid machine:'$machine'"

install_conf()
{
    test $# -eq 2 || die "invalid install_conf arguments:$*"

    src=$machine_dir/$1
    dst=$2

    test -f $src || return 0

    # two files are the same, do nothing
    test $(readlink_r $src) = $(readlink_r $dst) && { echo $src and $dst are the same file;return 0; }

    if test -f $dst;then
	echo "back up old file:$dst"
	cp -i $dst $dst.bak
    fi

    mkdir -p $(dirname $dst)
    echo will link $dst to $src
    ln -sfn $src $dst
}


for dot_file in $machine_dir/dotfiles/*
do
    dot_file=$(basename $dot_file)
    install_conf dotfiles/$dot_file ~/.$dot_file
done

if test -f $machine_dir/install.sh; then
    . $machine_dir/install.sh
fi

