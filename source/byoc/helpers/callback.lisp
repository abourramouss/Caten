;; Note1: only macOS should need this
;; Note2: create a closure that calls the stored callback (ensure it is thread-safe!)
;; Note3: this thing can be realized with only using cffi?
(c
"#ifdef __APPLE__
#include <stdio.h>

typedef void (*callback_t)(void);
static callback_t stored_cb = NULL;

void* closure_cffi_callback(void (*lisp_callback)(void)) {
    stored_cb = lisp_callback;
    return (^{ stored_cb(); });
}

#else

#include <stdio.h>

void* closure_cffi_callback(void (*lisp_callback)(void)) {
    fprintf(stderr, \"Error(caten/byoc/helper/callback.lisp): Apple Blocks extension not supported on this platform.\n\");
    return NULL; 
}

#endif
")
