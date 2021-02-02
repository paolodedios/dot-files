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
    #
    # macOS searches for the Java SDK in /Library/Java/JavaVirtualMachines/
    #
    darwin*)
        #
        # OpenJDK 11.x
        #
        # @see https://ports.macports.org/port/openjdk11/summary
        #
        function select_jdk11()
        {
            export JAVA_HOME=$(/usr/libexec/java_home -v 11)
        }
        #
        # Java SE 8
        #
        # @see https://ports.macports.org/port/openjdk8/summary
        #
        function select_jdk8()
        {
            export JAVA_HOME=$(/usr/libexec/java_home -v 1.8.0)
        }

        # Default to Java 11
        select_jdk11

        # Set GRADLE_HOME
        export GRADLE_HOME=/opt/local/share/java/gradle

        ;;

    linux*)
        ;;
esac
