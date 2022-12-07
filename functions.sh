function tailSysLog {
    tail -f /var/log/syslog -n 100
}

function weather {
    if [ $# -eq 0 ] # If no argument has been passed to this function
    then
        curl wttr.in
    else
        curl wttr.in/"$1" # Append location
    fi
}

function getOSXVersion {
    sw_vers | grep ProductVersion | cut -d: -f2
}

function getPidAndHtopByProcessName {
    local -r PROCESS_NAME=$1
    export TSPID=$(ps -ef | grep $1 | head -1 | awk '{print $2}'); echo $TSPID; htop -p $TSPID
}

function record-terminal-gif {
    echo "https://github.com/sassman/t-rec-rs: not installed yet" > /dev/stderr
}

function listCDs {
    wodim --devices
}

function getPidByProcessName {
    pidof $1
}

function killProcessThatKeepsFile {
    fuser -k $1
}

function countThreadsForProcessByName {
    ps uH p `pidof $1` | wc -l
}

function getNetworkActivity {
    lsof -r 2 -p `pidof $1` -i -a
}

function getFilesOpenedByProcess {
    lsof -c $1
}

function whoKeepsFile {
    fuser -v $1
}

function mountIso {
    mount $1 /mnt/cdrom -oloop
}

function listFilesInCurrentDir {
    du -sh * | sort -n
}

function downloadWholeSite {
    get --random-wait -r -p -e robots=off -U $1 $2
}

function randStr {
    local N=${1:5}
    echo `base64 </dev/urandom | tr -dc 'a-zA-Z0-9' | head -c$N`
}

function whoListenPort {
    lsof -i :$1
}

function shareCurrentFolder {
    python -m http.server
}

function getExternalIp {
    curl ifconfig.me
}

function takeScreenVideo {
    ffmpeg -f x11grab -s wxga -r 25 -i :0.0 -sameq ~/temp/desktop.mpg
}

function shareShellOutput {
    # nc -k - live after client disconnect
    # nc -l by default
    SHELL=/bin/bash
    mkfifo /tmp/fifo;(nc -q0 -l 5000 < /tmp/fifo > /dev/null &);script -f /tmp/fifo && rm /tmp/fifo
}

function listAllOpenPortsAndTheirOwningExecutables {
    lsof -i -P | grep -i "listen"
}

function findPPARepository {
    # ppasearch utility
    [ -z "$1" ] && echo "usage: `basename $0` \"search string\"" 1>&2 && exit 1

    wget -qO- "https://launchpad.net/ubuntu/+ppas?name_filter=$1" |\
    sed -ne 's/^.\+=\"\(.\+\)\">.\+<\/a><\/td>.*$/https:\/\/launchpad.net\1/p'
}

function diffRemoteSites {
    diff <(wget -q -O - $1) <(wget -q -O - $2)
}
#bind a key with function
#bind -x '"\C-l":ls -l'

function viewDeviceInformation {
    sudo file -s /dev/sd*
}

function changePrimaryDisplay {
    xrandr --output DVI1 --primary
}

function changePrimaryDisplayX {
    xrandr --output DVI1 --primary --mode 1440x900
}

function findDefaultMonitor {
    xrandr --prop | grep "[^dis]connected" | cut --delimiter=" " -f1
}

function makeVirtualDisplay {
    xrandr --output DVI1 --primary --mode 1440x900 --left-of VGA1
}

function logBash {
    script /tmp/log.txt
}


function med {
    git ls-files --modified | grep $1 | xargs -i{} $2 {};
}

function showInterfaceRateInKbPerSec {
    while [ /bin/true ]; do OLD=$NEW; NEW=`cat /proc/net/dev | grep eth0 | tr -s ' ' | cut -d' ' -f "3 11"`; echo $NEW $OLD | awk '{printf("\rin: % 9.2g\t\tout: % 9.2g", ($1-$3)/1024, ($2-$4)/1024)}'; sleep 1; done
}

function showResourcesForProcess {
    if [ -z "${1}" ]; then
        echo "show CPU usage, RSS and VSZ memory for processes by name";
        echo "usage: pxr <process_name>";
        return;
    fi;
    for i in `ps -aef|grep ${1}|grep -v grep|grep -v ps|awk '{print $2}'`;
    do
        ps -o pid -o pcpu -o rss -o vsz -o cmd ${i};
    done
}

# monitor memory usage
function monitorMemoryUsage {
    watch vmstat -sSM
}

function file_charset {
    file -i $0
}

function reloadXdefaults {
    xrdb ~/.Xdefaults
}

function showInternetActivityAtTheMoment {
    lsof -P -i -n
}

# siege - stress testing for the web site
# ngrep - light-weight wireshark
function makeSnapshotOfPipe {
    ipconfig | convert label:@- ip.png
}

function trueuniq {
    awk '{ if (!h[$0]) { print $0; h[$0]=1 } }'
}

# youtube-dl has this functionality built in. If you're running an older version
# of youtube-dl, you can update it using `youtube-dl -U`
function convertUtubeVideoToMp3 {
    local -r VIDEO_URL="$1"
    youtube-dl -t --extract-audio --audio-format mp3 $VIDEO_URL
}

# start command and kill it if still running after 5 secs
# > timeout 5s <command>

# fc opens the last command in $EDITOR and runs the altered version afterwards)
# parralel

function monitorPortConnections {
    while true ; do sleep 1 ; clear ; (netstat -tn | grep -P ':22s+d') ; done
}

function transmitFileLikeHttpServer {
    # Allow to launch nc like a daemon, in background until you still
    # stop it.
    # You can stop it with kill %1 (jobs method) or kill PID.
    # The -k option can force nc to listen another connection, but if
    # you use redirection, it will work only one time.
    # The loop's inside doesn't do anything, but we can imagine to
    # send a message to screen when a connection is established

    while ( nc -l 80 < $1 > : ) ; do : ; done &
}

function printTcpConnectionStatistic {
    netstat -npat|grep ESTABLISHED | awk 'BEGIN{counter=0;} {split($5,a,":");ip=a[1];if(ip in ips)ips[ip]+=1;else ips[ip]=1;counter++;} END{for(i in ips)print i" = "ips[i]"\n"}'| sort | grep -vP '^\s*$'
}

function is_grub_installed {
    sudo dd bs=512 count=1 if=$1 | od -Ax -tx1z -v
}

function ec {
    emacsclient -t $1
}

function emacsOpen {
   open -n $(dirname $(dirname $(readlink -f `which emacs`)))/Applications/Emacs.app
}

# mac osx
# system_profiler -xml SPHardwareDataType


function pretty_print_xml  {
    cat $1 | python -c 'import sys; import xml.dom.minidom; s=sys.stdin.read(); print(xml.dom.minidom.parseString(s).toprettyxml())'
}

function simple_chat {
    ncat -vlm 5 --ssl --chat 9876
}

function docker-enter {
    docker exec -it $1 /bin/bash
}

function docker-container-ip {
    # takes container ID as a single parameter
    docker inspect -f '{{.NetworkSettings.IPAddress}}' $1
}

function pylint_on_changed {
    git st | grep ".py$" | cut -d" " -f3 | xargs -I{} pylint -r n -f colorized --rcfile=./pylint.cfg {}
}

function show_process_stack {
    sudo gdb -batch -ex bt -p $@
}

function list_all_opened_ports {
    lsof -Pan -i tcp -i udp
}

function link_environment_file_to_postactivate {
     rm $VIRTUAL_ENV/bin/postactivate; ln -s `pwd`/.environment $VIRTUAL_ENV/bin/postactivate
}

function java_use() {
    export JAVA_HOME=$(/usr/libexec/java_home -v $1)
    export PATH=$JAVA_HOME/bin:$PATH
    java -version
}

function activate-nix-profile {
    source ~/.nix-profile/etc/profile.d/nix.sh
}

function nix-show-package-info {
    nix show-derivation $1
}

function docker-remove-all-containers {
    docker rm `docker ps -a -q`
}

function install-anaconda-dependencies {
    wget -o ~/tmp/anaconda-requirements.txt https://raw.githubusercontent.com/proofit404/anaconda-mode/master/requirements.txt
    pip install -r ~/tmp/anaconda-requirements.txt
}


function ssh-copy-id {
    cat ~/.ssh/id_rsa.pub | ssh $1 "mkdir -p ~/.ssh && cat >>  ~/.ssh/authorized_keys"
}

function download-youtube-playlist {
    youtube-dl -f best --yes-playlist $1
}

function create-tox-project {
    cookiecutter https://github.com/signalpillar/cookiecutter-tox-based.git
}

function docker-ocaml-playground {
    docker run -it -v /Users/signalpillar/Google\ Drive/notebooks:/root/notebooks -P oh-my-ocaml
}

function docker-jupyter {
    docker run -v ~/Google\ Drive/notebooks:/home/jovyan/work --rm -p 8888:8888 jupyter/datascience-notebook
}

function cdd {
    local PATH=$1
    /bin/mkdir -p $PATH
    cd $PATH
}


function kubectl-service-version {
  local SERVICE_NAME=$1
  kubectl get pods -l app=$SERVICE_NAME -o json | jq '.items[].spec.containers[].env[] | select(.name | contains("SERVICE_VERSION"))'
}

function battery_stat_osx {
    pmset -g batt
}

function enable_indexing_osx {
    sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist
}

function disable_indexing_osx {
    sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist
}


# -----------------------------------------------
DEFAULT_ANDROID_SDK_DIR=~/Library/Android/sdk
# list SDK: sdkmanager --list
# install : sdkmanager --install 'system-images;android-25;google_apis;armeabi-v7a'

# https://stackoverflow.com/questions/53076422/getting-android-sdkmanager-to-run-with-java-11
function __android_download_jaxb_lib {
    local -r sdk_dir=${1:-$DEFAULT_ANDROID_SDK_DIR}
    mkdir $sdk_dir/jaxb_lib
    wget https://repo1.maven.org/maven2/javax/activation/activation/1.1.1/activation-1.1.1.jar -O $sdk_dir/jaxb_lib/activation.jar
    wget https://repo1.maven.org/maven2/com/sun/xml/bind/jaxb-impl/2.3.3/jaxb-impl-2.3.3.jar -O $sdk_dir/jaxb_lib/jaxb-impl.jar
    wget https://repo1.maven.org/maven2/com/sun/istack/istack-commons-runtime/3.0.11/istack-commons-runtime-3.0.11.jar -O $sdk_dir/jaxb_lib/istack-commons-runtime.jar
    wget https://repo1.maven.org/maven2/org/glassfish/jaxb/jaxb-xjc/2.3.3/jaxb-xjc-2.3.3.jar -O $sdk_dir/jaxb_lib/jaxb-xjc.jar
    wget https://repo1.maven.org/maven2/org/glassfish/jaxb/jaxb-core/2.3.0.1/jaxb-core-2.3.0.1.jar -O $sdk_dir/jaxb_lib/jaxb-core.jar
    wget https://repo1.maven.org/maven2/org/glassfish/jaxb/jaxb-jxc/2.3.3/jaxb-jxc-2.3.3.jar -O $sdk_dir/jaxb_lib/jaxb-jxc.jar
    wget https://repo1.maven.org/maven2/javax/xml/bind/jaxb-api/2.3.1/jaxb-api-2.3.1.jar -O $sdk_dir/jaxb_lib/jaxb-api.jar

}

function __set_android_classpath_for_java11 {
    # the classpath needed only for Java11
    local -r JAXLIBDIR=~/Library/Android/sdk/jaxb_lib

    if [[ $CLASSPATH != *"jaxb_lib"* ]]; then
        export CLASSPATH=$JAXLIBDIR/activation.jar:$JAXLIBDIR/jaxb-impl.jar:$JAXLIBDIR/jaxb-xjc.jar:$JAXLIBDIR/jaxb-core.jar:$JAXLIBDIR/jaxb-jxc.jar:$JAXLIBDIR/jaxb-api.jar:$CLASSPATH
    fi
    # avdmanager/sdkmanager have to be updated to take into account an existing CLASSPATH
}

function android-start-emulator {
    local -r NAME=$1
    # /usr/local/share/android-sdk/emulator/emulator -avd $NAME
    __set_android_classpath_for_java11
    local -r sdk_dir=${2:-$DEFAULT_ANDROID_SDK_DIR}
    $sdk_dir/emulator/emulator -avd $NAME -no-snapshot-load
}

function android-create-emulator-kitkat {
    local -r NAME=$1
    __set_android_classpath_for_java11
    avdmanager --verbose create avd -n $NAME -k "system-images;android-21;google_apis;x86" --tag google_apis -d pixel
}

function android-create-emulator {
    local -r NAME=$1
    __set_android_classpath_for_java11
    avdmanager --verbose create avd -n $NAME -k "system-images;android-30;google_apis;x86" --tag google_apis --sdcard 2048M -d pixel
}

function android-create-emulator-arm {
    local -r NAME=$1
    __set_android_classpath_for_java11
    # avdmanager --verbose create avd -n $NAME -k "system-images;android-25;android-wear;armeabi-v7a" --tag google_apis --sdcard 2048M -d pixel
    echo no | avdmanager --verbose create avd --force --name $NAME --abi 'google_apis/armeabi-v7a' --package 'system-images;android-25;google_apis;armeabi-v7a'
}

function android-delete-avd {
  local -r NAME=$1
  __set_android_classpath_for_java11
  avdmanager delete avd --name $NAME
}

function android-enable-cool-stuff {
  local -r NAME=$1
  echo "\nhw.keyboard=yes\nhw.ramSize=4096" >> ~/.android/avd/$NAME.avd/config.ini
  echo "[UPDATED] ~/.android/avd/$NAME.avd/config.ini"
}

function bmi_w_h {
    local -r weight=$1
    local -r height=$2
    echo "BMI: $(bc <<< "scale=3; $weight / (($height/100) * ($height/100))")"
}

function osx_ldd {
    otool -L $@
}

function git-clone-project {
    local -r https_url=$1
    cd ~/proj
    path_without_schema=$(echo $https_url | grep / | cut -d/ -f2-)
    proj_path=$(dirname "$HOME/proj/$path_without_schema")
    mkdir -p $proj_path
    cd $proj_path
    git clone $https_url
}

function debug-http-server {
    local -r port=${1:-8004}
    echo "Start debug server on port '$port' that will respond with 200 status code"
    for i in `seq 5`; do { echo -e "HTTP/1.1 200 OK\r\n\n{\"nc_response_number\": $i}";  } | nc -l 8004; done
}


function devider {
    # https://iterm2.com/documentation-images.html
    if [ $# -eq 0 ]; then
        echo "Usage: divider file"
        exit 1
    fi
    printf '\033]1337;File=inline=1;width=100%%;height=1;preserveAspectRatio=0'
    printf ":"
    base64 < "$1"
    printf '\a\n'
}

function try_py_package {
    local -r env_name=$1
    local -r pip_install_args=$2
    cd ~/tmp
    python -m venv "$env_name"
    source "$env_name"/bin/activate
    pip install $pip_install_args
}

# 1password functions
function op_save_file {
    local -r filename=$1
    local -r title=$2
    op create document $filename --title $title
}

function op_get_file {
    local -r title=$1
    op get document $title
}
