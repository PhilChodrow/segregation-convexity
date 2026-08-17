
mutual_information <- function(df){
    # Compute the joint probability distribution
    joint_prob <- df / sum(df)
    
    # Compute the marginal distributions
    row_prob <- rowSums(joint_prob)
    col_prob <- colSums(joint_prob)

    # Compute the mutual information
    mi <- 0
    for(i in seq_len(nrow(joint_prob))){
        for(j in seq_len(ncol(joint_prob))){
            if(joint_prob[i, j] > 0){
                mi <- mi + joint_prob[i, j] * log(joint_prob[i, j] / (row_prob[i] * col_prob[j]))
            }
        }
    }
    return(mi)
}