type error = string Loc.spanned

exception Unexpected of error
exception Expected of error
exception NameError of error
exception TypeError of error
