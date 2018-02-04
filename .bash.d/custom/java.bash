#!/bin/bash
#
# Java/JDK environment variables
#
########################################################################################

########################################################################################
# Java Decompiler (JAD) helper command
########################################################################################

# Execute JAD with standard options
function jadexec()
{
    if [ -e $(type -p jad) ]; then
        find . -name \*.class |xargs jad -b -d "$1" -dead -ff -i -o -r -radix10 -s .java -safe -stat
    else
        echo "ERROR: Java Decompiler not found."
    fi
}

# Execute JAD with fully qualified names and with verbose processing output
function jadexecv()
{
    if [ -e $(type -p jad) ]; then
        find . -name \*.class |xargs jad -b -d "$1" -dead -f -ff -i -o -r -radix10 -s .java -safe -stat -v
    else
        echo "ERROR: Java Decompiler not found."
    fi
}


########################################################################################
# Define OS specific functions
########################################################################################

case $OSTYPE in
    darwin*)

        function select_jdk6()
        {
            export JAVA_HOME=$(/usr/libexec/java_home -v 1.6.0)
        }

        function select_jdk7()
        {
            export JAVA_HOME=$(/usr/libexec/java_home -v 1.7.0)
        }

        function select_jdk8()
        {
            export JAVA_HOME=$(/usr/libexec/java_home -v 1.8.0)
        }

        # Default to Java/JDK 8
        select_jdk8

        # Set GRADLE_HOME
        export GRADLE_HOME=/opt/local/share/java/gradle

        ;;

    linux*)
        ;;
esac
