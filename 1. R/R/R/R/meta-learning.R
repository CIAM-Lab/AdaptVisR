#' Meta-Learning for Model Selection
#'
#' @description
#' Implements the meta-learning-based model selection described in Section 3.2
#' of Ye & Qin (MICCAI 2025). Reduces selection time from weeks to minutes.
#'
#' @param target_data Target domain data (features)
#' @param repository Model repository (list of pretrained models)
#' @param top_k Number of top models to return
#'
#' @return Recommended models with scores
#' @export
#'
#' @examples
#' \dontrun{
#' recommended <- meta_select(new_domain_data, pretrained_models())
#' }
meta_select <- function(target_data, repository, top_k = 3) {
  
  # Extract meta-features from target data
  meta_features <- extract_meta_features(target_data)
  
  # Score each model in repository
  scores <- sapply(repository, function(model) {
    score_model(model, meta_features)
  })
  
  # Get top-k models
  top_indices <- order(scores, decreasing = TRUE)[1:min(top_k, length(scores))]
  
  recommendations <- lapply(top_indices, function(i) {
    list(
      model = repository[[i]],
      score = scores[i],
      meta_features = attr(repository[[i]], "meta_features")
    )
  })
  
  attr(recommendations, "query_meta_features") <- meta_features
  class(recommendations) <- "meta_recommendations"
  
  return(recommendations)
}

#' Extract meta-features from data
#'
#' @param x Input data
#'
#' @return List of meta-features
#' @keywords internal
extract_meta_features <- function(x) {
  
  # Convert to matrix if needed
  x_mat <- as.matrix(x)
  
  # Check if moments package is available for skewness/kurtosis
  has_moments <- requireNamespace("moments", quietly = TRUE)
  
  # Compute statistical meta-features
  features <- list(
    n_samples = nrow(x_mat),
    n_features = ncol(x_mat),
    mean = mean(x_mat, na.rm = TRUE),
    sd = sd(x_mat, na.rm = TRUE),
    sparsity = mean(abs(x_mat) < 1e-8, na.rm = TRUE),
    estimated_complexity = min(20, ncol(x_mat) / 10)
  )
  
  # Add skewness and kurtosis if moments package is available
  if (has_moments) {
    features$skewness <- moments::skewness(as.vector(x_mat))
    features$kurtosis <- moments::kurtosis(as.vector(x_mat))
  } else {
    features$skewness <- 0
    features$kurtosis <- 3
    warning("Package 'moments' not installed. Using default values for skewness/kurtosis.")
  }
  
  return(features)
}

#' Score a model against target meta-features
#'
#' @param model Model from repository
#' @param target_meta Target meta-features
#'
#' @return Similarity score
#' @keywords internal
score_model <- function(model, target_meta) {
  
  model_meta <- attr(model, "meta_features")
  if (is.null(model_meta)) {
    # Default score if model has no meta-features
    return(0.5)
  }
  
  # Compute similarity on common features
  common_names <- intersect(names(target_meta), names(model_meta))
  
  if (length(common_names) == 0) {
    return(0.5)
  }
  
  # Simple scoring - in practice use learned similarity metric
  score <- 0
  weight_sum <- 0
  
  for (name in common_names) {
    if (is.numeric(target_meta[[name]]) && is.numeric(model_meta[[name]])) {
      # Assign weights based on feature importance
      weight <- switch(name,
        "n_samples" = 0.1,
        "n_features" = 0.2,
        "mean" = 0.15,
        "sd" = 0.15,
        "skewness" = 0.1,
        "kurtosis" = 0.1,
        "sparsity" = 0.1,
        "estimated_complexity" = 0.2,
        0.1  # default weight
      )
      
      # Normalize difference
      max_val <- max(abs(target_meta[[name]]), abs(model_meta[[name]]), 1)
      diff <- abs(target_meta[[name]] - model_meta[[name]])
      similarity <- 1 - min(diff / max_val, 1)
      
      score <- score + weight * similarity
      weight_sum <- weight_sum + weight
    }
  }
  
  # Normalize by total weight
  if (weight_sum > 0) {
    score <- score / weight_sum
  }
  
  return(score)
}

#' Get pretrained model repository
#'
#' @return List of pretrained models with meta-features
#' @export
pretrained_models <- function() {
  
  # Placeholder for actual pretrained models
  models <- list()
  
  # Medical domain models
  models$medical_resnet <- structure(
    list(
      name = "ResNet-50 (Medical)",
      domain = "medical",
      architecture = "resnet50",
      pretrained_on = "NIH ChestX-ray"
    ),
    meta_features = list(
      n_features = 2048,
      mean = 0.2,
      sd = 0.8,
      skewness = 0.5,
      kurtosis = 3.2,
      sparsity = 0.1,
      estimated_complexity = 15
    )
  )
  
  # Industrial domain models
  models$industrial_efficientnet <- structure(
    list(
      name = "EfficientNet-V2 (Industrial)",
      domain = "industrial",
      architecture = "efficientnetv2",
      pretrained_on = "MVTec AD"
    ),
    meta_features = list(
      n_features = 1280,
      mean = 0.3,
      sd = 0.7,
      skewness = 0.3,
      kurtosis = 2.8,
      sparsity = 0.15,
      estimated_complexity = 12
    )
  )
  
  # Agricultural domain models
  models$agricultural_vit <- structure(
    list(
      name = "ViT (Agricultural)",
      domain = "agricultural",
      architecture = "vit_base",
      pretrained_on = "PlantVillage"
    ),
    meta_features = list(
      n_features = 768,
      mean = 0.25,
      sd = 0.75,
      skewness = 0.4,
      kurtosis = 3.0,
      sparsity = 0.2,
      estimated_complexity = 18
    )
  )
  
  # Security domain models
  models$security_dinov2 <- structure(
    list(
      name = "DINOv2 (Security)",
      domain = "security",
      architecture = "dinov2",
      pretrained_on = "UA-DETRAC"
    ),
    meta_features = list(
      n_features = 1024,
      mean = 0.28,
      sd = 0.72,
      skewness = 0.35,
      kurtosis = 2.9,
      sparsity = 0.12,
      estimated_complexity = 16
    )
  )
  
  # General domain model
  models$general_clip <- structure(
    list(
      name = "CLIP (General)",
      domain = "general",
      architecture = "clip_vit",
      pretrained_on = "LAION-2B"
    ),
    meta_features = list(
      n_features = 512,
      mean = 0.3,
      sd = 0.7,
      skewness = 0.4,
      kurtosis = 3.1,
      sparsity = 0.08,
      estimated_complexity = 20
    )
  )
  
  return(models)
}

#' Print recommendations
#'
#' @param x Recommendations from meta_select
#' @param ... Additional arguments
#'
#' @export
print.meta_recommendations <- function(x, ...) {
  cat("Meta-Learning Model Recommendations\n")
  cat("====================================\n")
  cat(sprintf("Query meta-features:\n"))
  qf <- attr(x, "query_meta_features")
  cat(sprintf("  Samples: %d\n", qf$n_samples))
  cat(sprintf("  Features: %d\n", qf$n_features))
  cat(sprintf("  Mean: %.3f\n", qf$mean))
  cat(sprintf("  SD: %.3f\n", qf$sd))
  if (!is.null(qf$skewness)) {
    cat(sprintf("  Skewness: %.3f\n", qf$skewness))
    cat(sprintf("  Kurtosis: %.3f\n", qf$kurtosis))
  }
  cat(sprintf("  Sparsity: %.3f\n", qf$sparsity))
  cat(sprintf("  Estimated complexity: %.1f\n", qf$estimated_complexity))
  
  cat("\nTop recommendations:\n")
  for (i in seq_along(x)) {
    cat(sprintf("\n%d. %s (score: %.3f)\n", 
                i, x[[i]]$model$name, x[[i]]$score))
    cat(sprintf("    Domain: %s | Architecture: %s\n", 
                x[[i]]$model$domain, x[[i]]$model$architecture))
    cat(sprintf("    Pretrained on: %s\n", x[[i]]$model$pretrained_on))
  }
}

#' Plot model recommendation scores
#'
#' @param recommendations Output from meta_select
#'
#' @return ggplot object
#' @export
plot_recommendations <- function(recommendations) {
  
  if (!inherits(recommendations, "meta_recommendations")) {
    stop("Input must be a meta_recommendations object")
  }
  
  # Extract data for plotting
  df <- data.frame(
    model = sapply(recommendations, function(x) x$model$name),
    score = sapply(recommendations, function(x) x$score),
    domain = sapply(recommendations, function(x) x$model$domain)
  )
  
  # Create bar plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(model, score), y = score, fill = domain)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Model Recommendation Scores",
      x = NULL,
      y = "Score",
      fill = "Domain"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}
