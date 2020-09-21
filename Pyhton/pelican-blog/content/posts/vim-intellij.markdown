title: Opening intellij source in vim
slug: intellij-vim
category: computing
date: 2019-05-09
modified: 2019-05-09
<!-- Status: draft -->



```
/run/current-system/sw/bin/urxvt
```

```
-e sh -c "vim --servername "intellij" --remote +$LineNumber$ $FilePath$"
```

```
$ProjectFileDir$
```
