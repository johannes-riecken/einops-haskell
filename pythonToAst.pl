#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

while (<DATA>) {
    s/^x = //;
    s/tf\.(\w+)\(x, /\u$1 /;
    s/\)$//;
    s/_(\w+)/ "$1"/;
    if ($. != 1) {
        print ', ';
    }
    tr/()/[]/;
    print;
}

__DATA__
x = tf.reshape(x, [4, 4, 3])
x = tf.transpose(x, [0, 1, 2])
tf.reshape(x, [4, 4, 3])
