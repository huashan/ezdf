
#    if your package has a NAMESPACE, then .onLoad() is where you do this
#
#    if your package does not have NAMESPACE, then .First.lib() is where you do this
#
#    either way, use packageStartupMessage() instead of cat() so that users have a choice of suppressing this.

.onLoad<-function(libname, pkgname) {
  assign("ez_globals", new.env(), envir=parent.env(environment()))
  init_hooks()

}


.onAttach<-function(libname, pkgname) {
  # called by attach()
}