#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(all);

# Exit 0 when every file meets both thresholds, 1 when a report was produced but
# a file is below one, 2 when no report could be produced at all.
my $EXIT_CANNOT_RUN = 2;

my @COLUMNS = qw(file line-covered line-total branch-covered branch-total
                 uncovered-lines);
my $EXCLUSIONS = 'scripts/coverage-exclusions.sexp';
my $SOURCE_ROOT = 'src';
my %ENVIRONMENT = (
  input        => 'COVERAGE_SUMMARY',
  'min-line'   => 'CL_ORG_COVERAGE_MIN_LINE',
  'min-branch' => 'CL_ORG_COVERAGE_MIN_BRANCH',
);
my %DEFAULT = (
  input        => '/tmp/cl-org-kit-coverage/per-file.tsv',
  'min-line'   => 100,
  'min-branch' => 100,
);
# Each pair names a kind and the option holding its minimum. Index 0 selects the
# line columns 2 and 3 of a summary row, index 1 the branch columns 4 and 5.
my @KINDS = (['line', 'min-line'], ['branch', 'min-branch']);

sub fail {
  my ($message) = @_;
  print STDERR "$0: $message\n";
  exit $EXIT_CANNOT_RUN;
}

sub usage {
  print STDERR <<"USAGE";
usage: $0 [--input TSV] [--min-line PERCENT] [--min-branch PERCENT]
             [--source-root DIRECTORY] [--exclusions FILE]

  --input TSV          per-file coverage summary (default: \$COVERAGE_SUMMARY,
                       else /tmp/cl-org-kit-coverage/per-file.tsv)
  --min-line PERCENT   required line coverage (default:
                       \$CL_ORG_COVERAGE_MIN_LINE, else 100)
  --min-branch PERCENT required branch coverage (default:
                       \$CL_ORG_COVERAGE_MIN_BRANCH, else 100)
  --source-root DIRECTORY source files used to classify exclusions (default: src)
  --exclusions FILE    exclusion data (default: scripts/coverage-exclusions.sexp)

exit 0 when every file meets both thresholds, 1 when a file is below one, and 2
when the settings are invalid or the summary cannot be read.
USAGE
  exit $EXIT_CANNOT_RUN;
}

sub fail_usage {
  my ($message) = @_;
  print STDERR "$0: $message\n\n";
  usage();
}

# A set-but-empty environment variable is how a shell spells "unset", while an
# empty option value is a mistake and stays visible to the validator.
sub resolve {
  my ($setting, $given) = @_;
  my $environment = $ENV{$ENVIRONMENT{$setting}};
  return ($given, 'option') if defined $given;
  return ($environment, $ENVIRONMENT{$setting})
    if defined $environment && length $environment;
  return ($DEFAULT{$setting}, 'default');
}

sub resolve_percent {
  my ($setting, $given) = @_;
  my ($raw, $origin) = resolve($setting, $given);
  my $number = $raw =~ /\A(?:\d+(?:\.\d+)?|\.\d+)\z/ ? $raw : undef;
  fail("--$setting ($origin) must be a percentage in 0..100, got '$raw'")
    if !defined $number || $number > 100;
  return 0 + $number;
}

sub parse_arguments {
  my %given;
  while (@ARGV) {
    my $argument = shift @ARGV;
    my ($setting) = $argument =~ /\A--([a-z][a-z-]*)\z/;
    fail_usage("unexpected argument '$argument'") unless defined $setting;
    fail_usage("unknown option --$setting")
      unless exists $DEFAULT{$setting}
          || $setting eq 'source-root' || $setting eq 'exclusions';
    fail_usage("option --$setting requires a value") unless @ARGV;
    $given{$setting} = shift @ARGV;
  }
  return %given;
}

sub exclusion_rules {
  my ($path) = @_;
  open my $handle, '<', $path or fail("cannot read exclusions $path: $!");
  my @rules;
  while (my $line = <$handle>) {
    next if $line =~ /\A\s*(?:;|\z)/;
    my ($kind, $head, $reproduction)
      = $line =~ /:kind\s+([^\s()]+).*?:head\s+"([^"]+)".*?:reproduction\s+"([^"]+)"/;
    fail("$path: expected :kind and :head in '$line'")
      unless defined $kind && defined $head && defined $reproduction;
    fail("$path: missing reproduction $reproduction") unless -f $reproduction;
    push @rules, { kind => $kind, head => $head, reproduction => $reproduction };
  }
  close $handle or fail("cannot read exclusions $path: $!");
  fail("$path has no exclusion rules") unless @rules;
  return \@rules;
}

sub source_code_lines {
  my ($lines) = @_;
  my @code = map { '' } @$lines;
  my $source = join '', @$lines;
  my ($line, $offset) = (0, 0);
  my ($in_string, $string_escape, $in_symbol, $symbol_escape, $line_comment)
    = (0, 0, 0, 0, 0);
  my $block_comment_depth = 0;

  while ($offset < length($source)) {
    my $character = substr($source, $offset, 1);
    if ($character eq "\n") {
      $code[$line++] .= "\n";
      $line_comment = 0;
      $string_escape = 0 if $in_string;
      $symbol_escape = 0 if $in_symbol;
      $offset++;
      next;
    }

    if ($line_comment) {
      $code[$line] .= ' ';
      $offset++;
      next;
    }

    if ($block_comment_depth) {
      my $pair = substr($source, $offset, 2);
      if ($pair eq '#|') {
        $block_comment_depth++;
        $code[$line] .= '  ';
        $offset += 2;
      } elsif ($pair eq '|#') {
        $block_comment_depth--;
        $code[$line] .= '  ';
        $offset += 2;
      } else {
        $code[$line] .= ' ';
        $offset++;
      }
      next;
    }

    if ($in_string) {
      if ($string_escape) {
        $string_escape = 0;
      } elsif ($character eq '\\') {
        $string_escape = 1;
      } elsif ($character eq '"') {
        $in_string = 0;
      }
      $code[$line] .= ' ';
      $offset++;
      next;
    }

    if ($in_symbol) {
      if ($symbol_escape) {
        $symbol_escape = 0;
        $code[$line] .= 'x';
      } elsif ($character eq '\\') {
        $symbol_escape = 1;
        $code[$line] .= 'x';
      } elsif ($character eq '|') {
        $in_symbol = 0;
        $code[$line] .= '|';
      } else {
        $code[$line] .= 'x';
      }
      $offset++;
      next;
    }

    my $pair = substr($source, $offset, 2);
    if ($pair eq '#\\') {
      $code[$line] .= '  ';
      $offset += 2;
      if ($offset < length($source)) {
        my $literal = substr($source, $offset, 1);
        if ($literal =~ /[A-Za-z]/) {
          while ($offset < length($source)
                 && substr($source, $offset, 1) =~ /[A-Za-z0-9-]/) {
            $code[$line] .= ' ';
            $offset++;
          }
        } else {
          if ($literal eq "\n") {
            $code[$line++] .= "\n";
          } else {
            $code[$line] .= ' ';
          }
          $offset++;
        }
      }
    } elsif ($pair eq '#|') {
      $block_comment_depth = 1;
      $code[$line] .= '  ';
      $offset += 2;
    } elsif ($character eq '"') {
      $in_string = 1;
      $code[$line] .= ' ';
      $offset++;
    } elsif ($character eq ';') {
      $line_comment = 1;
      $code[$line] .= ' ';
      $offset++;
    } elsif ($character eq '|') {
      $in_symbol = 1;
      $code[$line] .= '|';
      $offset++;
    } elsif ($character eq '\\') {
      $code[$line] .= 'x';
      $offset++;
      if ($offset < length($source)) {
        if (substr($source, $offset, 1) eq "\n") {
          $code[$line++] .= "\n";
        } else {
          $code[$line] .= 'x';
        }
        $offset++;
      }
    } else {
      $code[$line] .= $character;
      $offset++;
    }
  }

  return \@code;
}

sub defun_header_end_line {
  my ($lines, $start) = @_;
  my $text = join '', @$lines[$start .. $#$lines];
  pos($text) = 0;
  fail("cannot locate DEFUN header at line " . ($start + 1))
    unless $text =~ /\G\s*\(\s*(?:defun|defmacro)\b/gc;
  $text =~ /\G\s*/gc;
  my $name_character = substr($text, pos($text), 1);
  if ($name_character eq '(') {
    my $name_depth = 0;
    while (pos($text) < length($text)) {
      my $character = substr($text, pos($text), 1);
      pos($text)++;
      $name_depth++ if $character eq '(';
      $name_depth-- if $character eq ')';
      last if $name_depth == 0;
    }
  } elsif ($name_character eq '|') {
    pos($text)++;
    my $closed = 0;
    while (pos($text) < length($text)) {
      my $character = substr($text, pos($text), 1);
      pos($text)++;
      if ($character eq '\\') {
        pos($text)++ if pos($text) < length($text);
      } elsif ($character eq '|') {
        $closed = 1;
        last;
      }
    }
    fail("unterminated escaped DEFUN name at line " . ($start + 1))
      unless $closed;
  } else {
    my $name_start = pos($text);
    while (pos($text) < length($text)) {
      my $character = substr($text, pos($text), 1);
      last if $character =~ /\s/ || $character eq '(' || $character eq ')';
      if ($character eq '\\') {
        pos($text)++;
        pos($text)++ if pos($text) < length($text);
      } else {
        pos($text)++;
      }
    }
    fail("cannot locate DEFUN name at line " . ($start + 1))
      if pos($text) == $name_start;
  }
  $text =~ /\G\s*/gc;
  fail("cannot locate DEFUN lambda list at line " . ($start + 1))
    unless substr($text, pos($text), 1) eq '(';

  my $depth = 0;
  my $closing;
  for (my $index = pos($text); $index < length($text); $index++) {
    my $character = substr($text, $index, 1);
    $depth++ if $character eq '(';
    if ($character eq ')') {
      $depth--;
      if ($depth == 0) {
        $closing = $index;
        last;
      }
    }
  }
  fail("unterminated DEFUN lambda list at line " . ($start + 1))
    unless defined $closing;

  my $before = substr($text, 0, $closing);
  my $line = $start + 1 + (() = $before =~ /\n/g);
  my $line_end = index($text, "\n", $closing);
  $line_end = length($text) if $line_end < 0;
  my $after = substr($text, $closing + 1, $line_end - $closing - 1);
  return $line - 1 if $after =~ /\S/;
  return $line;
}

sub source_exclusion_lines {
  my ($path, $rules) = @_;
  open my $handle, '<', $path or fail("cannot read source $path: $!");
  my @lines = <$handle>;
  close $handle or fail("cannot read source $path: $!");
  my $code = source_code_lines(\@lines);
  my %allowed = map { $_->{head} => 1 }
    grep { $_->{kind} eq 'top-level-form' } @$rules;
  my @excluded;
  my $depth = 0;
  my $start;
  for my $index (0 .. $#lines) {
    my $clean = $code->[$index];
    my $countable = $clean;
    my $opens = () = $countable =~ /\(/g;
    my $closes = () = $countable =~ /\)/g;
    if ($depth == 0 && $clean =~ /\(\s*([A-Za-z0-9*+!?_-]+)/) {
      $start = $index + 1;
      my $head = $1;
      if ($allowed{$head}) {
        my $balance = 0;
        my $end = $index;
        for my $j ($index .. $#lines) {
          my $part = $code->[$j];
          $balance += (() = $part =~ /\(/g);
          $balance -= (() = $part =~ /\)/g);
          if ($balance <= 0) {
            $end = $j;
            last;
          }
        }
        push @excluded, [$start, $end + 1, $head];
      } elsif ($head eq 'defun' || $head eq 'defmacro') {
        if (grep { $_->{kind} eq 'defun-header' } @$rules) {
          my $header_end = defun_header_end_line($code, $index);
          push @excluded, [$start, $header_end, $head]
            if $header_end >= $start;
        }
      }
    }
    $depth += $opens - $closes;
  }
  return \@excluded;
}

sub line_is_excluded {
  my ($line, $ranges) = @_;
  return 1 if grep { $line >= $_->[0] && $line <= $_->[1] } @$ranges;
  return 0;
}

sub read_rows {
  my ($path) = @_;
  open my $handle, '<', $path or fail("cannot read $path: $!");
  my (@rows, $number, $header);
  while (my $line = <$handle>) {
    $number++;
    next if $line =~ /\A#/;
    chomp $line;
    my @fields = split /\t/, $line, -1;
    unless ($header) {
      fail("$path line $number: expected header '"
           . join("\t", @COLUMNS) . "', got '$line'")
        unless "@fields" eq "@COLUMNS";
      $header = 1;
      next;
    }
    fail("$path line $number: expected " . scalar(@COLUMNS)
         . " tab-separated columns, got " . scalar(@fields))
      if @fields != @COLUMNS;
    fail("$path line $number: file must not be empty")
      if $fields[0] =~ /\A\s*\z/;
    my @counts;
    for my $column (1 .. 4) {
      fail("$path line $number: $COLUMNS[$column] must be a non-negative"
           . " integer, got '$fields[$column]'")
        unless $fields[$column] =~ /\A\d+\z/;
      push @counts, 0 + $fields[$column];
    }
    fail("$path line $number: line-covered cannot exceed line-total")
      if $fields[1] > $fields[2];
    fail("$path line $number: branch-covered cannot exceed branch-total")
      if $fields[3] > $fields[4];
    my $uncovered = $fields[5];
    fail("$path line $number: invalid uncovered-lines syntax '$uncovered'")
      unless $uncovered eq '-'
          || $uncovered =~ /\A\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*\z/;
    if ($uncovered ne '-') {
      for my $range (split /,/, $uncovered) {
        my ($start, $end) = split /-/, $range;
        $end = $start unless defined $end;
        fail("$path line $number: uncovered-lines range '$range' is backwards")
          if $start < 1 || $end < $start;
      }
    }
    push @rows, { file => $fields[0], counts => \@counts,
                  uncovered => $uncovered };
  }
  close $handle or fail("cannot read $path: $!");
  fail("$path is empty") unless $number;
  fail("$path has no header row") unless $header;
  fail("$path has no data rows") unless @rows;
  return \@rows;
}

sub percentage {
  my ($covered, $total) = @_;
  return $total ? 100 * $covered / $total : undef;
}

sub percent_text {
  my ($value) = @_;
  return 'n/a' unless defined $value;
  return sprintf '%.1f%%', $value;
}

my %given = parse_arguments();
my ($input) = resolve('input', $given{input});
my $source_root = $given{'source-root'} // $SOURCE_ROOT;
my $exclusions = $given{exclusions} // $EXCLUSIONS;
my @minimum
  = map { resolve_percent($KINDS[$_][1], $given{ $KINDS[$_][1] }) } 0 .. $#KINDS;
my $rows = read_rows($input);
my $rules = exclusion_rules($exclusions);

my (@failures, @totals);
for my $row (@$rows) {
  my $ranges = [];
  my $source = "$source_root/$row->{file}";
  $ranges = source_exclusion_lines($source, $rules) if -f $source;
  $row->{excluded} = $ranges;
  my @values;
  for my $index (0 .. $#KINDS) {
    my ($covered, $total) = @{$row->{counts}}[2 * $index, 2 * $index + 1];
    push @values, percentage($covered, $total);
    $totals[2 * $index] += $covered;
    $totals[2 * $index + 1] += $total;
  }
  $row->{values} = \@values;
  my @reasons;
  for my $index (0 .. $#KINDS) {
    next unless defined $values[$index];
    next if $values[$index] >= $minimum[$index];
    push @reasons, sprintf '%s %s, required %s', $KINDS[$index][0],
      percent_text($values[$index]), percent_text($minimum[$index]);
  }
  if (@reasons) {
    my $uncovered = $row->{uncovered};
    my $branch_is_below_minimum
      = defined $values[1] && $values[1] < $minimum[1];
    # The summary has no branch locations, so line exclusions cannot justify
    # a branch coverage deficit.
    my $residual_is_excluded = !$branch_is_below_minimum
      && $uncovered ne '-'
      && all { my $line = $_; line_is_excluded($line, $ranges) }
           map { my ($start, $end) = split /-/, $_; $end //= $start;
                 ($start .. $end) } split /,/, $uncovered;
    push @failures, { file => $row->{file}, reasons => \@reasons }
      unless $residual_is_excluded;
  }
}

print "| file | line | branch | uncovered lines |\n";
print "|---|---:|---:|---|\n";
for my $row (@$rows) {
  print sprintf "| %s | %s | %s | %s |\n", $row->{file},
    percent_text($row->{values}[0]), percent_text($row->{values}[1]),
    $row->{uncovered};
}
print sprintf "| total | %s | %s |  |\n",
  percent_text(percentage($totals[0], $totals[1])),
  percent_text(percentage($totals[2], $totals[3]));

if (@failures) {
  print "\n";
  for my $failure (@failures) {
    print "  $failure->{file}: ", join('; ', @{$failure->{reasons}}), "\n";
  }
  printf "coverage gate: FAIL (%d of %d files below threshold)\n",
    scalar @failures, scalar @$rows;
  exit 1;
}
printf "coverage gate: PASS (all %d files at or above threshold)\n",
  scalar @$rows;
exit 0;
