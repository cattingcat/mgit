#include <git2.h>
#include <git2/status.h>

int git_status_foreach_integr(
	git_repository *repo,
	git_status_cb callback,
	void *payload);

int git_status_list_new_integr(
    git_status_list **out,
    git_repository *repo);

int git_fetch_init_options_integr(
    git_fetch_options **opts);

int git_checkout_head_integr(
    git_repository *repo);

int git_checkout_tree_integr(
    git_repository *repo,
    const git_object *treeish);
