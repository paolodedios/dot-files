#!/usr/bin/env bats

load ../test_utils

load ../../lib/composure
load ../../plugins/available/base.plugin

cite _about _param _example _group _author _version

load ../../lib/helpers
load ../../lib/search

NO_COLOR=true

function local_setup {
  mkdir -p "$BASH_IT"
  lib_directory="$(cd "$(dirname "$0")" && pwd)"
  # Use rsync to copy Bash-it to the temp folder
  # rsync is faster than cp, since we can exclude the large ".git" folder
  rsync -qavrKL -d --delete-excluded --exclude=.git $lib_directory/../../../.. "$BASH_IT"

  rm -rf "$BASH_IT"/enabled
  rm -rf "$BASH_IT"/aliases/enabled
  rm -rf "$BASH_IT"/completion/enabled
  rm -rf "$BASH_IT"/plugins/enabled
}

@test "search: plugin base" {
  run _bash-it-search-component 'plugins' 'base'
  [[ "${lines[0]}" =~ 'plugins' && "${lines[0]}" =~ 'base' ]]
}

@test "search: ruby gem bundle rake rails npm" {
  # first disable them all, so that  the output does not appear with a checkbox
  # and we can compare the result
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm' '--disable'
  # Now perform the search
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm'
  # And verify
  assert [ "${lines[0]/✓/}" == '      aliases  =>   bundler npm' ]
  assert [ "${lines[1]/✓/}" == '      plugins  =>   ruby' ]
  assert [ "${lines[2]/✓/}" == '  completions  =>   bundler gem npm rake' ]
}

@test "search: ruby gem bundle rake rails -npm" {
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm' '--disable'
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' '-npm'
  assert [ "${lines[0]/✓/}" == '      aliases  =>   bundler' ]
  assert [ "${lines[1]/✓/}" == '      plugins  =>   ruby' ]
  assert [ "${lines[2]/✓/}" == '  completions  =>   bundler gem rake' ]
}

@test "search: (npm enabled) ruby gem bundle rake rails npm" {
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm' '--disable'
  run _enable-alias 'npm'
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm'
  assert_line -n 0 '      aliases  =>   bundler ✓npm'
  assert_line -n 1 '      plugins  =>   ruby'
  assert_line -n 2 '  completions  =>   bundler gem npm rake'
}

@test "search: (all enabled) ruby gem bundle rake rails npm" {
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm' '--enable'
  run _bash-it-search 'ruby' 'gem' 'bundle' 'rake' 'rails' 'npm'
  assert_line -n 0 '      aliases  =>   ✓bundler ✓npm'
  assert_line -n 1 '      plugins  =>   ✓ruby'
  assert_line -n 2 '  completions  =>   ✓bundler ✓gem ✓npm ✓rake'
}
