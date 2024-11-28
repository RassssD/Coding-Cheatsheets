
// Programs in Stata - a simple template

cap program drop prog_example // Need to do this before redefining to update the code
program define prog_example

    syntax varlist(), arg_1(string) arg_2(int) [optional_arg_1(string) optional_arg_2]

    // Do operations in a separate frame
    * Get current user frame
    local frame_user = c(frame)

    * switch into new frame
    cap frame drop frame_prog
    frame create frame_prog
    put , into(frame_prog)


    






end



