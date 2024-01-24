function homegit --wraps=git --description 'Access homegit repository'
  git --git-dir=$HOME/hearth/git/homegit --work-tree=$HOME $argv
end
