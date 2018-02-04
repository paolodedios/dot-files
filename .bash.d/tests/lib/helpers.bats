#!/usr/bin/env bats

load ../test_utils
load ../../lib/composure
load ../../plugins/available/base.plugin

cite _about _param _example _group _author _version

load ../../lib/helpers

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

  mkdir -p "$BASH_IT"/enabled
  mkdir -p "$BASH_IT"/aliases/enabled
  mkdir -p "$BASH_IT"/completion/enabled
  mkdir -p "$BASH_IT"/plugins/enabled
}

# TODO Create global __is_enabled function
# TODO Create global __get_base_name function
# TODO Create global __get_enabled_name function

@test "helpers: _command_exists function exists" {
  type -a _command_exists &> /dev/null
  assert_success
}

@test "helpers: _command_exists function positive test ls" {
  run _command_exists ls
  assert_success
}

@test "helpers: _command_exists function positive test bash-it" {
  run _command_exists bash-it
  assert_success
}

@test "helpers: _command_exists function negative test" {
  run _command_exists __addfkds_dfdsjdf
  assert_failure
}

@test "helpers: bash-it help aliases ag" {
  run bash-it help aliases "ag"
  assert_line -n 0 "ag='ag --smart-case --pager=\"less -MIRFX'"
}

@test "helpers: bash-it help aliases without any aliases enabled" {
  run bash-it help aliases
  assert_line -n 0 ""
}

@test "helpers: bash-it help list aliases without any aliases enabled" {
  run _help-list-aliases "$BASH_IT/aliases/available/ag.aliases.bash"
  assert_line -n 0 "ag:"
}

@test "helpers: bash-it help list aliases with ag aliases enabled" {
  ln -s $BASH_IT/aliases/available/ag.aliases.bash $BASH_IT/aliases/enabled/150---ag.aliases.bash
  assert_link_exist "$BASH_IT/aliases/enabled/150---ag.aliases.bash"

  run _help-list-aliases "$BASH_IT/aliases/enabled/150---ag.aliases.bash"
  assert_line -n 0 "ag:"
}

@test "helpers: bash-it help list aliases with python aliases enabled" {
  ln -s $BASH_IT/aliases/available/python.aliases.bash $BASH_IT/aliases/enabled/150---python.aliases.bash
  assert_link_exist "$BASH_IT/aliases/enabled/150---python.aliases.bash"

  run _help-list-aliases "$BASH_IT/aliases/enabled/150---python.aliases.bash"
  assert_line -n 0 "python:"
}

@test "helpers: bash-it help list aliases with docker-compose aliases enabled" {
  ln -s $BASH_IT/aliases/available/docker-compose.aliases.bash $BASH_IT/aliases/enabled/150---docker-compose.aliases.bash
  assert_link_exist "$BASH_IT/aliases/enabled/150---docker-compose.aliases.bash"

  run _help-list-aliases "$BASH_IT/aliases/enabled/150---docker-compose.aliases.bash"
  assert_line -n 0 "docker-compose:"
}

@test "helpers: bash-it help list aliases with ag aliases enabled in global directory" {
  ln -s $BASH_IT/aliases/available/ag.aliases.bash $BASH_IT/enabled/150---ag.aliases.bash
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"

  run _help-list-aliases "$BASH_IT/enabled/150---ag.aliases.bash"
  assert_line -n 0 "ag:"
}

@test "helpers: bash-it help aliases one alias enabled in the old directory" {
  ln -s $BASH_IT/aliases/available/ag.aliases.bash $BASH_IT/aliases/enabled/150---ag.aliases.bash
  assert_link_exist "$BASH_IT/aliases/enabled/150---ag.aliases.bash"

  run bash-it help aliases
  assert_line -n 0 "ag:"
}

@test "helpers: bash-it help aliases one alias enabled in global directory" {
  run bash-it enable alias "ag"
  assert_line -n 0 'ag enabled with priority 150.'
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"

  run bash-it enable plugin "aws"
  assert_line -n 0 'aws enabled with priority 250.'
  assert_link_exist "$BASH_IT/enabled/250---aws.plugin.bash"

  run bash-it help aliases
  assert_line -n 0 "ag:"
  assert_line -n 1 "ag='ag --smart-case --pager=\"less -MIRFX'"
}

@test "helpers: enable the python aliases through the bash-it function" {
  run bash-it enable alias "python"
  assert_line -n 0 'python enabled with priority 150.'
  assert_link_exist "$BASH_IT/enabled/150---python.aliases.bash"
}

@test "helpers: enable the curl aliases" {
  run _enable-alias "curl"
  assert_line -n 0 'curl enabled with priority 150.'
  assert_link_exist "$BASH_IT/enabled/150---curl.aliases.bash"
}

@test "helpers: enable the apm completion through the bash-it function" {
  run bash-it enable completion "apm"
  assert_line -n 0 'apm enabled with priority 350.'
  assert_link_exist "$BASH_IT/enabled/350---apm.completion.bash"
}

@test "helpers: enable the git completion" {
  run _enable-completion "git"
  assert_line -n 0 'git enabled with priority 350.'
  assert_link_exist "$BASH_IT/enabled/350---git.completion.bash"
}

@test "helpers: enable the docker plugin" {
  run _enable-plugin "docker"
  assert_line -n 0 'docker enabled with priority 250.'
  assert_link_exist "$BASH_IT/enabled/250---docker.plugin.bash" "../plugins/available/docker.plugin.bash"
}

@test "helpers: enable the docker plugin through the bash-it function" {
  run bash-it enable plugin "docker"
  assert_line -n 0 'docker enabled with priority 250.'
  assert_link_exist "$BASH_IT/enabled/250---docker.plugin.bash"
}

@test "helpers: enable the fasd and fzf plugins through the bash-it function" {
  run bash-it enable plugin "fasd" "fzf"
  assert_line -n 0 'fasd enabled with priority 250.'
  assert_line -n 1 'fzf enabled with priority 375.'
  assert_link_exist "$BASH_IT/enabled/250---fasd.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
}

@test "helpers: enable the foo-unkown and fz plugins through the bash-it function" {
  run bash-it enable plugin "foo-unknown" "fzf"
  assert_line -n 0 'sorry, foo-unknown does not appear to be an available plugin.'
  assert_line -n 1 'fzf enabled with priority 375.'
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
}

@test "helpers: enable the fzf plugin" {
  run _enable-plugin "fzf"
  assert_line -n 0 'fzf enabled with priority 375.'
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
}

@test "helpers: enable an unknown plugin" {
  run _enable-plugin "unknown-foo"
  assert_line -n 0 'sorry, unknown-foo does not appear to be an available plugin.'

  # Check for both old an new structure
  assert [ ! -L "$BASH_IT/plugins/enabled/250---unknown-foo.plugin.bash" ]
  assert [ ! -L "$BASH_IT/plugins/enabled/unknown-foo.plugin.bash" ]

  assert [ ! -L "$BASH_IT/enabled/250---unknown-foo.plugin.bash" ]
  assert [ ! -L "$BASH_IT/enabled/unknown-foo.plugin.bash" ]
}

@test "helpers: enable an unknown plugin through the bash-it function" {
  run bash-it enable plugin "unknown-foo"
  echo "${lines[@]}"
  assert_line -n 0 'sorry, unknown-foo does not appear to be an available plugin.'

  # Check for both old an new structure
  assert [ ! -L "$BASH_IT/plugins/enabled/250---unknown-foo.plugin.bash" ]
  assert [ ! -L "$BASH_IT/plugins/enabled/unknown-foo.plugin.bash" ]

  assert [ ! -L "$BASH_IT/enabled/250---unknown-foo.plugin.bash" ]
  assert [ ! -L "$BASH_IT/enabled/unknown-foo.plugin.bash" ]
}

@test "helpers: disable a plugin that is not enabled" {
  run _disable-plugin "sdkman"
  assert_line -n 0 'sorry, sdkman does not appear to be an enabled plugin.'
}

@test "helpers: enable and disable the fzf plugin" {
  run _enable-plugin "fzf"
  assert_line -n 0 'fzf enabled with priority 375.'
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
  assert [ ! -L "$BASH_IT/plugins/enabled/375---fzf.plugin.bash" ]

  run _disable-plugin "fzf"
  assert_line -n 0 'fzf disabled.'
  assert [ ! -L "$BASH_IT/enabled/375---fzf.plugin.bash" ]
}

@test "helpers: disable the fzf plugin if it was enabled with a priority, but in the component-specific directory" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/375---fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/375---fzf.plugin.bash"
  assert [ ! -L "$BASH_IT/enabled/375---fzf.plugin.bash" ]

  run _disable-plugin "fzf"
  assert_line -n 0 'fzf disabled.'
  assert [ ! -L "$BASH_IT/plugins/enabled/375---fzf.plugin.bash" ]
  assert [ ! -L "$BASH_IT/enabled/375---fzf.plugin.bash" ]
}

@test "helpers: disable the fzf plugin if it was enabled without a priority" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"

  run _disable-plugin "fzf"
  assert_line -n 0 'fzf disabled.'
  assert [ ! -L "$BASH_IT/plugins/enabled/fzf.plugin.bash" ]
}

@test "helpers: enable the fzf plugin if it was enabled without a priority" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"

  run _enable-plugin "fzf"
  assert_line -n 0 'fzf is already enabled.'
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"
  assert [ ! -L "$BASH_IT/plugins/enabled/375---fzf.plugin.bash" ]
  assert [ ! -L "$BASH_IT/enabled/375---fzf.plugin.bash" ]
}

@test "helpers: enable the fzf plugin if it was enabled with a priority, but in the component-specific directory" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/375---fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/375---fzf.plugin.bash"

  run _enable-plugin "fzf"
  assert_line -n 0 'fzf is already enabled.'
  assert [ ! -L "$BASH_IT/plugins/enabled/fzf.plugin.bash" ]
  assert_link_exist "$BASH_IT/plugins/enabled/375---fzf.plugin.bash"
  assert [ ! -L "$BASH_IT/enabled/375---fzf.plugin.bash" ]
}

@test "helpers: enable the fzf plugin twice" {
  run _enable-plugin "fzf"
  assert_line -n 0 'fzf enabled with priority 375.'
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"

  run _enable-plugin "fzf"
  assert_line -n 0 'fzf is already enabled.'
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
}

@test "helpers: migrate plugins and completions that share the same name" {
  ln -s $BASH_IT/completion/available/dirs.completion.bash $BASH_IT/completion/enabled/350---dirs.completion.bash
  assert_link_exist "$BASH_IT/completion/enabled/350---dirs.completion.bash"

  ln -s $BASH_IT/plugins/available/dirs.plugin.bash $BASH_IT/plugins/enabled/250---dirs.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/250---dirs.plugin.bash"

  run _bash-it-migrate
  assert_line -n 0 'Migrating plugin dirs.'
  assert_line -n 1 'dirs disabled.'
  assert_line -n 2 'dirs enabled with priority 250.'
  assert_line -n 3 'Migrating completion dirs.'
  assert_line -n 4 'dirs disabled.'
  assert_line -n 5 'dirs enabled with priority 350.'
  assert_line -n 6 'If any migration errors were reported, please try the following: reload && bash-it migrate'

  assert_link_exist "$BASH_IT/enabled/350---dirs.completion.bash"
  assert_link_exist "$BASH_IT/enabled/250---dirs.plugin.bash"
  assert [ ! -L "$BASH_IT/completion/enabled/350----dirs.completion.bash" ]
  assert [ ! -L "$BASH_IT/plugins/enabled/250----dirs.plugin.bash" ]
}

@test "helpers: migrate enabled plugins that don't use the new priority-based configuration" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"

  ln -s $BASH_IT/plugins/available/fasd.plugin.bash $BASH_IT/plugins/enabled/fasd.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fasd.plugin.bash"

  ln -s $BASH_IT/aliases/available/python.aliases.bash $BASH_IT/aliases/enabled/python.aliases.bash
  assert_link_exist "$BASH_IT/aliases/enabled/python.aliases.bash"

  run _enable-plugin "sshagent"
  assert_link_exist "$BASH_IT/enabled/250---sshagent.plugin.bash"

  run _bash-it-migrate
  assert_line -n 0 'Migrating alias python.'
  assert_line -n 1 'python disabled.'
  assert_line -n 2 'python enabled with priority 150.'

  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/250---fasd.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/250---sshagent.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/150---python.aliases.bash"
  assert [ ! -L "$BASH_IT/plugins/enabled/fasd.plugin.bash" ]
  assert [ ! -L "$BASH_IT/plugins/enabled/fzf.plugin.bash" ]
  assert [ ! -L "$BASH_IT/aliases/enabled/python.aliases.bash" ]
}

@test "helpers: migrate enabled plugins that use the new priority-based configuration in the individual directories" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/375---fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/375---fzf.plugin.bash"

  ln -s $BASH_IT/plugins/available/fasd.plugin.bash $BASH_IT/plugins/enabled/250---fasd.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/250---fasd.plugin.bash"

  ln -s $BASH_IT/aliases/available/python.aliases.bash $BASH_IT/aliases/enabled/250---python.aliases.bash
  assert_link_exist "$BASH_IT/aliases/enabled/250---python.aliases.bash"

  run _enable-plugin "sshagent"
  assert_link_exist "$BASH_IT/enabled/250---sshagent.plugin.bash"

  run _bash-it-migrate
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/250---fasd.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/250---sshagent.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/150---python.aliases.bash"
  assert [ ! -L "$BASH_IT/plugins/enabled/225----fasd.plugin.bash" ]
  assert [ ! -L "$BASH_IT/plugins/enabled/250----fzf.plugin.bash" ]
  assert [ ! -L "$BASH_IT/aliases/enabled/250----python.aliases.bash" ]
}

@test "helpers: run the migrate command without anything to migrate and nothing enabled" {
  run _bash-it-migrate
}

@test "helpers: run the migrate command without anything to migrate" {
  run _enable-plugin "sshagent"
  assert_link_exist "$BASH_IT/enabled/250---sshagent.plugin.bash"

  run _bash-it-migrate
  assert_link_exist "$BASH_IT/enabled/250---sshagent.plugin.bash"
}

function __migrate_all_components() {
  subdirectory="$1"
  one_type="$2"
  priority="$3"

  for f in "${BASH_IT}/$subdirectory/available/"*.bash
  do
    to_enable=$(basename $f)
    if [ -z "$priority" ]; then
      ln -s "../available/$to_enable" "${BASH_IT}/${subdirectory}/enabled/$to_enable"
    else
      ln -s "../available/$to_enable" "${BASH_IT}/${subdirectory}/enabled/$priority---$to_enable"
    fi
  done

  ls ${BASH_IT}/${subdirectory}/enabled

  all_available=$(compgen -G "${BASH_IT}/${subdirectory}/available/*.$one_type.bash" | wc -l | xargs)
  all_enabled_old=$(compgen -G "${BASH_IT}/${subdirectory}/enabled/*.$one_type.bash" | wc -l | xargs)

  assert_equal "$all_available" "$all_enabled_old"

  run bash-it migrate

  all_enabled_old_after=$(compgen -G "${BASH_IT}/${subdirectory}/enabled/*.$one_type.bash" | wc -l | xargs)
  assert_equal "0" "$all_enabled_old_after"

  all_enabled_new_after=$(compgen -G "${BASH_IT}/enabled/*.$one_type.bash" | wc -l | xargs)
  assert_equal "$all_enabled_old" "$all_enabled_new_after"
}

@test "helpers: migrate all plugins" {
  subdirectory="plugins"
  one_type="plugin"

  __migrate_all_components "$subdirectory" "$one_type"
}

@test "helpers: migrate all aliases" {
  subdirectory="aliases"
  one_type="aliases"

  __migrate_all_components "$subdirectory" "$one_type"
}

@test "helpers: migrate all completions" {
  subdirectory="completion"
  one_type="completion"

  __migrate_all_components "$subdirectory" "$one_type"
}

@test "helpers: migrate all plugins with previous priority" {
  subdirectory="plugins"
  one_type="plugin"

  __migrate_all_components "$subdirectory" "$one_type" "100"
}

@test "helpers: migrate all aliases with previous priority" {
  subdirectory="aliases"
  one_type="aliases"

  __migrate_all_components "$subdirectory" "$one_type" "100"
}

@test "helpers: migrate all completions with previous priority" {
  subdirectory="completion"
  one_type="completion"

  __migrate_all_components "$subdirectory" "$one_type" "100"
}

@test "helpers: verify that existing components are automatically migrated when something is enabled" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"

  run bash-it enable plugin "fasd"
  assert_line -n 0 'Migrating plugin fzf.'
  assert_line -n 1 'fzf disabled.'
  assert_line -n 2 'fzf enabled with priority 375.'
  assert_line -n 3 'If any migration errors were reported, please try the following: reload && bash-it migrate'
  assert_line -n 4 'fasd enabled with priority 250.'
  assert [ ! -L "$BASH_IT/plugins/enabled/fzf.plugin.bash" ]
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
  assert_link_exist "$BASH_IT/enabled/250---fasd.plugin.bash"
}

@test "helpers: verify that existing components are automatically migrated when something is disabled" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"
  ln -s $BASH_IT/plugins/available/fasd.plugin.bash $BASH_IT/plugins/enabled/250---fasd.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/250---fasd.plugin.bash"

  run bash-it disable plugin "fasd"
  assert_line -n 0 'Migrating plugin fasd.'
  assert_line -n 1 'fasd disabled.'
  assert_line -n 2 'fasd enabled with priority 250.'
  assert_line -n 3 'Migrating plugin fzf.'
  assert_line -n 4 'fzf disabled.'
  assert_line -n 5 'fzf enabled with priority 375.'
  assert_line -n 6 'If any migration errors were reported, please try the following: reload && bash-it migrate'
  assert_line -n 7 'fasd disabled.'
  assert [ ! -L "$BASH_IT/plugins/enabled/fzf.plugin.bash" ]
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"
  assert [ ! -L "$BASH_IT/plugins/enabled/250---fasd.plugin.bash" ]
  assert [ ! -L "$BASH_IT/enabled/250---fasd.plugin.bash" ]
}

@test "helpers: enable all plugins" {
  run _enable-plugin "all"
  local available=$(find $BASH_IT/plugins/available -name *.plugin.bash | wc -l | xargs)
  local enabled=$(find $BASH_IT/enabled -name [0-9]*.plugin.bash | wc -l | xargs)
  assert_equal "$available" "$enabled"
}

@test "helpers: disable all plugins" {
  run _enable-plugin "all"
  local available=$(find $BASH_IT/plugins/available -name *.plugin.bash | wc -l | xargs)
  local enabled=$(find $BASH_IT/enabled -name [0-9]*.plugin.bash | wc -l | xargs)
  assert_equal "$available" "$enabled"

  run _enable-alias "ag"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"

  run _disable-plugin "all"
  local enabled2=$(find $BASH_IT/enabled -name [0-9]*.plugin.bash | wc -l | xargs)
  assert_equal "0" "$enabled2"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"
}

@test "helpers: disable all plugins in the old directory structure" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"

  ln -s $BASH_IT/plugins/available/fasd.plugin.bash $BASH_IT/plugins/enabled/fasd.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fasd.plugin.bash"

  local enabled=$(find $BASH_IT/plugins/enabled -name *.plugin.bash | wc -l | xargs)
  assert_equal "2" "$enabled"

  run _enable-alias "ag"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"

  run _disable-plugin "all"
  local enabled2=$(find $BASH_IT/plugins/enabled -name *.plugin.bash | wc -l | xargs)
  assert_equal "0" "$enabled2"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"
}

@test "helpers: disable all plugins in the old directory structure with priority" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/250---fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/250---fzf.plugin.bash"

  ln -s $BASH_IT/plugins/available/fasd.plugin.bash $BASH_IT/plugins/enabled/250---fasd.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/250---fasd.plugin.bash"

  local enabled=$(find $BASH_IT/plugins/enabled -name *.plugin.bash | wc -l | xargs)
  assert_equal "2" "$enabled"

  run _enable-alias "ag"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"

  run _disable-plugin "all"
  local enabled2=$(find $BASH_IT/plugins/enabled -name *.plugin.bash | wc -l | xargs)
  assert_equal "0" "$enabled2"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"
}

@test "helpers: disable all plugins without anything enabled" {
  local enabled=$(find $BASH_IT/enabled -name [0-9]*.plugin.bash | wc -l | xargs)
  assert_equal "0" "$enabled"

  run _enable-alias "ag"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"

  run _disable-plugin "all"
  local enabled2=$(find $BASH_IT/enabled -name [0-9]*.plugin.bash | wc -l | xargs)
  assert_equal "0" "$enabled2"
  assert_link_exist "$BASH_IT/enabled/150---ag.aliases.bash"
}

@test "helpers: enable the ansible aliases through the bash-it function" {
  run bash-it enable alias "ansible"
  assert_line -n 0 'ansible enabled with priority 150.'
  assert_link_exist "$BASH_IT/enabled/150---ansible.aliases.bash"
}

@test "helpers: describe the fzf plugin without enabling it" {
  _bash-it-plugins | grep "fzf" | grep "\[ \]"
}

@test "helpers: describe the fzf plugin after enabling it" {
  run _enable-plugin "fzf"
  assert_line -n 0 'fzf enabled with priority 375.'
  assert_link_exist "$BASH_IT/enabled/375---fzf.plugin.bash"

  _bash-it-plugins | grep "fzf" | grep "\[x\]"
}

@test "helpers: describe the fzf plugin after enabling it in the old directory" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/fzf.plugin.bash"

  _bash-it-plugins | grep "fzf" | grep "\[x\]"
}

@test "helpers: describe the fzf plugin after enabling it in the old directory with priority" {
  ln -s $BASH_IT/plugins/available/fzf.plugin.bash $BASH_IT/plugins/enabled/375---fzf.plugin.bash
  assert_link_exist "$BASH_IT/plugins/enabled/375---fzf.plugin.bash"

  _bash-it-plugins | grep "fzf" | grep "\[x\]"
}

@test "helpers: describe the python aliases without enabling them" {
  run _bash-it-aliases
  assert_line "python                [ ]     python/virtualenv abbreviations"
}
