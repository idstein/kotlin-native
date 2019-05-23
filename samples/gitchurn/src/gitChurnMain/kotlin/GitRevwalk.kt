package sample.gitchurn


import kotlinx.cinterop.*
import libgit2.*
import platform.posix.alloca

/**
 * this class provides revwalk functionality without need of memory management etc
 */
class GitRevwalk (val repository_handle: CPointer<git_repository>, val sort: git_sort_t) {
    val arena = Arena()
    val handle: CPointer<git_revwalk> = memScoped {
        val loc = allocPointerTo<git_revwalk>()
        git_revwalk_new(loc.ptr, repository_handle).errorCheck()
        loc.value!!
    }

    fun close() {
        git_revwalk_free(handle)
        arena.clear()
    }

    fun repush(pushMode: String = "") { // I want to use pushMode as string,
                                        // that provides different regex or references for pushing
        git_revwalk_reset(handle)
        git_revwalk_sorting(walk = handle, sort_mode = sort)
        when(pushMode){
            "" -> git_revwalk_push_head(handle)
            else -> {
                val oid: CPointer<git_oid>? = null
                git_oid_fromstr(oid, pushMode)
                git_revwalk_push(handle, oid)}
        }
    }

    @ExperimentalUnsignedTypes
    fun sortMode(i:UInt){
        git_revwalk_sorting(walk = handle, sort_mode = i)
    }

    val oid: CPointer<git_oid>? // here i can take iterated oids pointing to different commits, sorted by ^
        get() {
            val o = arena.alloc<git_oid>()
            val n = git_revwalk_next(o.ptr, handle)
            when(n){
                0 -> return  o.ptr
                GIT_ITEROVER -> return  null
                else -> throw Exception("Unexpected result code $n")
            }
        }
}