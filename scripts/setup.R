dirs <- c("throughput", "fig")
for (d in dirs) {
    if (!dir.exists(d)) {
        dir.create(d)
    }
}
