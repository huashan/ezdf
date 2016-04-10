
#    if your package has a NAMESPACE, then .onLoad() is where you do this
#
#    if your package does not have NAMESPACE, then .First.lib() is where you do this
#
#    either way, use packageStartupMessage() instead of cat() so that users have a choice of suppressing this.

.onLoad<-function(libname, pkgname) {
  assign("ez_globals", new.env(), envir = parent.env(environment()))

  # if pander is loaded later
  setHook(packageEvent("pander", "attach"),
        function(...) {
          #message('hooked')
          .init_hooks()
        } )
	setHook(packageEvent("pander", "detach"),
        function(...) .uninit_hooks() )

  .init_hooks()
}

.onUnload <- function(libpath) {
  .uninit_hooks()
}

.onAttach<-function(libname, pkgname) {
  # called by attach()
}