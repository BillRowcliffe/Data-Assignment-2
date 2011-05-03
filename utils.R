## calculate the confusion matrix and accuracy
confusion = function(x, y) {
    tb = as.matrix(table(x, y))
    list(table = tb, tot.accuracy = sum(diag(tb))/sum(tb), 
	accuracies = diag(tb)/rowSums(tb))
}

split_data = function(prop = 2/3, factor) {

}
