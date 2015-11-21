#!/usr/bin/perl

#roster.parse.pl by Geoff Benn
#the purpose of this script is to convert UC Davis rosters (from the registrar) into
#a format that is more amenable for use as a whole-class roster or gradebook.
#to run the program call the script in the terminal and then specify the tab-delimited roster file to be parsed
#this would look like: perl roster.parse.pl Bio.roster.txt

#for complete instructions on how to use this script, please visit www.prospectiveprof.wordpress.com or watch the video
#at https://vimeo.com/108316346

use strict; use warnings;

my $roster = $ARGV[0];				#collects the name of the input file from the command line

open(IN, "<$roster");				#opens the input file
open(OUT, ">$roster.parsed.txt");	#creates an output file

my $crn = "null";			#variable that holds the crn (course registration menu)
my $section = "null";		#variable that holds the section number
my $title = 0;

#this line prints out a header
print OUT "crn\tsection\tseq\tSID\tlast\tfirst\tmiddle\tpreferred\tlevel\tunits\tclass\tmajor\tgrade\tstatus\tstatus date\temail\n";

#this loop opens the input file and looks for the information we want
while (<IN>) {
	chomp;												#removes whitespace characters at the end of each line in the input file
	my @line = split(/\t/);								#splits the input line into a tab-delimited array
	if ($title == 1) {									#if the previous line started with "Title", then the current line will have our CRN and section
		$crn = $line[1];								#collects the crn from the second cell
		$section = $line[5];							#collects the section from the sixth cell
		$title = 0;										#resets $title to 0
	}
	if ( exists($line[0])) {								#is there anything present in the line?
		if ($line[0] =~ m/TITLE/) {							#Asks if the first cell of the line has the word "Title"
			$title = 1;										#sets the $title variable to 1
		}
		if ( exists ($line[1])) {							#is there anything in the second cell?
			if (($line[0] =~ m/\d/) and ($line[1] =~ m/\d/)) {	#asks if we have values starting with numbers in both the first and second cells, if so its a student info line
				unshift(@line, $section);						#adds the section number to the front of the student information array
				unshift(@line, $crn);							#adds the crn to the front of the student information array
				my $print = join("\t",@line);					#converts the student info array to a tab-delimited string for printing
				print OUT "$print\n";							#prints the line to the output file
			}
		}
	}
}

close IN;
close OUT;
