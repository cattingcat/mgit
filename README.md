## mgit - Multi repo git

### Commands
`mgit branch`  Current branch in each repo
   
`mgit branches` Branches with number of repos

`mgit fetch`  Fetch all repos

`mgit lookup <branch or part of branch name>` Lookup branch with name

`mgit checkout <branch or part of branch name>` Checkout to specified branch in all repos which have this branch

`mgit repos` Repos in current folder  

### Dev notes
#### Install libs:

`brew install icu4c`

```
stack install text-icu \
 --extra-lib-dirs=/usr/local/opt/icu4c/lib \
 --extra-include-dirs=/usr/local/opt/icu4c/include
```

```
nm /usr/local/Cellar/libgit2/0.28.3/lib/libgit2.a > nm_out
```
