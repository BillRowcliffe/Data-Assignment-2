## calculate the confusion matrix and accuracy
confusion = function(x, y) {
    tb = table(x, y)
    list(table = tb, accuracy = sum(diag(tb))/sum(tb))
}
