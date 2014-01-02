if (( $+commands[anacron] )) ; then
    anacron -t ~/etc/anacrontab
fi
