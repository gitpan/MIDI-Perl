# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# Time-stamp: "1998-08-13 21:34:38 MDT"
######################### We start with some black magic to print on failure.

# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..13\n"; }
END {print "BAD! 1\n" unless $loaded;}
use MIDI;
$loaded = 1;
print "OK 1\n";

######################### End of black magic.
# Just need to test the BER now.
print( (pack("w", 0x00000000) eq "\x00") ? "OK" : "BAD!", " 2\n");
print( (pack("w", 0x00000040) eq "\x40") ? "OK" : "BAD!", " 3\n");
print( (pack("w", 0x0000007F) eq "\x7F") ? "OK" : "BAD!", " 4\n");
print( (pack("w", 0x00000080) eq "\x81\x00") ? "OK" : "BAD!", " 5\n");
print( (pack("w", 0x00002000) eq "\xC0\x00") ? "OK" : "BAD!", " 6\n");
print( (pack("w", 0x00003FFF) eq "\xFF\x7F") ? "OK" : "BAD!", " 7\n");
print( (pack("w", 0x00004000) eq "\x81\x80\x00") ? "OK" : "BAD!", " 8\n");
print( (pack("w", 0x00100000) eq "\xC0\x80\x00") ? "OK" : "BAD!", " 9\n");
print( (pack("w", 0x001FFFFF) eq "\xFF\xFF\x7F") ? "OK" : "BAD!", " 10\n");
print( (pack("w", 0x00200000) eq "\x81\x80\x80\x00") ? "OK" : "BAD!", " 11\n");
print( (pack("w", 0x08000000) eq "\xC0\x80\x80\x00") ? "OK" : "BAD!", " 12\n");
print( (pack("w", 0x0FFFFFFF) eq "\xFF\xFF\xFF\x7F") ? "OK" : "BAD!", " 13\n");
