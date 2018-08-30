                  Simple Testing Harness for JForlan

This directory contains:

  testing.sml
    a simple testing harness for JForlan

  tests/fa
    some FAs in Forlan syntax, ending in .txt

  tests/rfa
    some RFAs in Forlan syntax, ending in .txt

  tests/pt
    some parse trees in Forlan syntax, ending in .txt

  tests/reg
    some regular expressions in Forlan syntax, ending in .txt

  tests/prog
    some program trees in Forlan syntax, ending in .txt

To carry out the tests:

  cd to this directory, making sure both forlan and jforlan are on
  your shell's search path;

  invoke forlan;

  type
    use "testing.sml";

  type
    doit();

This will:

  input each object in tests/*/*.txt into Forlan;

  convert this Forlan value into concrete syntax, and feed it to
  JForlan;

  wait for the user to commit the object back to Forlan, in concrete
  syntax;

  load this concrete syntax into Forlan;

  and check that the resulting value is the same as the original
  Forlan value.

To get the full value from testing, one has to compare what JForlan
produces with the source.

One way to do negative testing is to take the example FAs, etc., make
changes to them by hand, and see what JForlan does when they are loaded.
