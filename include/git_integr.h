#include <git2.h>
#include <git2/status.h>

int git_status_foreach_integr(
	git_repository *repo,
	git_status_cb callback,
	void *payload);

int git_fetch_init_options_integr(
    git_fetch_options **opts);