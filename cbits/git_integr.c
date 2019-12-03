#include <git2.h>
#include <git2/status.h>
#include "git_integr.h"


int git_status_foreach_integr(git_repository *repo, git_status_cb callback, void *payload) {
    git_status_options *opts = malloc(sizeof(git_status_options));

    (*opts).show =
          GIT_STATUS_OPT_INCLUDE_UNTRACKED
        | GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS
        | GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX
        | GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR
        | GIT_STATUS_OPT_RENAMES_FROM_REWRITES;

    git_status_init_options(opts, GIT_STATUS_OPTIONS_VERSION);
    int r = git_status_foreach_ext(repo, opts, callback, payload);

    free(opts);
    return r;
}