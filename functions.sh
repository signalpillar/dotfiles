function tailSysLog {
    tail -f /var/log/syslog -n 100
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

function whoListenPort {
    lsof -i :$1
}

function shareCurrentFolder {
    python -m SimpleHTTPServer
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
    youtube-dl -t --extract-audio --audio-format mp3 $0
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

# mac osx
# system_profiler -xml SPHardwareDataType

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


function pixel {
    /usr/local/share/android-sdk/emulator/emulator -avd pixel
}

function start-android-emulator {
    local -r NAME=$1
    # /usr/local/share/android-sdk/emulator/emulator -avd $NAME
    /usr/local/share/android-sdk/emulator/emulator -avd $NAME -no-snapshot-load
}

function create-android-emulator-kitkat {
    local -r NAME=$1
    avdmanager --verbose create avd -n $NAME -k "system-images;android-19;google_apis;x86" --tag google_apis -d pixel
}

function create-android-emulator {
    local -r NAME=$1
    avdmanager --verbose create avd -n $NAME -k "system-images;android-27;google_apis;x86" --tag google_apis --sdcard 2048M -d pixel
}

function create-android-emulator-arm {
    local -r NAME=$1
    avdmanager --verbose create avd -n $NAME -k "system-images;android-25;google_apis;armeabi-v7a" --tag google_apis --sdcard 2048M -d pixel
}

function delete-android-avd {
  local -r NAME=$1
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
