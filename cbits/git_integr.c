#include <git2.h>
#include <git2/status.h>
#include <git2/remote.h>
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

int git_status_list_new_integr(git_status_list **out, git_repository *repo) {
    git_status_options *opts = malloc(sizeof(git_status_options));

    (*opts).show =
          GIT_STATUS_OPT_INCLUDE_UNTRACKED
        | GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS
        | GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX
        | GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR
        | GIT_STATUS_OPT_RENAMES_FROM_REWRITES;

    git_status_init_options(opts, GIT_STATUS_OPTIONS_VERSION);
    int r = git_status_list_new(out, repo, opts);

    free(opts);
    return r;
}

int git_fetch_init_options_integr(git_fetch_options **opts) {
    git_fetch_options *o = malloc(sizeof(git_fetch_options));
    (*opts) = o;

    int r = git_fetch_init_options(o, GIT_FETCH_OPTIONS_VERSION);

    // Download deletes of branches
    o->prune = GIT_FETCH_PRUNE;

    // Download all tags
    o->download_tags = GIT_REMOTE_DOWNLOAD_TAGS_ALL;

    return r;
}