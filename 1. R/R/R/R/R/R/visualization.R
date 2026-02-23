#' Visualization tools for CIAM
#'
#' @description
#' Visualization functions for attention maps, feature spaces,
#' and decision boundaries.
#'
#' @name visualization
NULL

#' Visualize causal vs domain features
#'
#' @param causal_features Causal features C (matrix or data frame)
#' @param domain_features Domain features D (matrix or data frame)
#' @param labels Class labels
#' @param method Dimensionality reduction method ("pca", "tsne", "umap")
#' @param title Plot title
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs
#' @importFrom stats prcomp
plot_feature_space <- function(causal_features, domain_features, labels,
                               method = c("pca", "tsne", "umap"),
                               title = "Feature Space Visualization") {
  
  method <- match.arg(method)
  
  # Check if required packages are available
  if (method == "tsne" && !requireNamespace("Rtsne", quietly = TRUE)) {
    warning("Package 'Rtsne' not installed. Falling back to PCA.")
    method <- "pca"
  }
  
  if (method == "umap" && !requireNamespace("umap", quietly = TRUE)) {
    warning("Package 'umap' not installed. Falling back to PCA.")
    method <- "pca"
  }
  
  # Ensure inputs are matrices
  causal_mat <- as.matrix(causal_features)
  domain_mat <- as.matrix(domain_features)
  
  # Apply dimensionality reduction
  if (method == "pca") {
    # PCA for causal features
    causal_pca <- stats::prcomp(causal_mat, center = TRUE, scale. = TRUE)
    causal_2d <- causal_pca$x[, 1:2]
    
    # PCA for domain features
    domain_pca <- stats::prcomp(domain_mat, center = TRUE, scale. = TRUE)
    domain_2d <- domain_pca$x[, 1:2]
    
  } else if (method == "tsne") {
    # t-SNE for causal features
    causal_tsne <- Rtsne::Rtsne(causal_mat, dims = 2, perplexity = min(30, nrow(causal_mat)/5))
    causal_2d <- causal_tsne$Y
    
    # t-SNE for domain features
    domain_tsne <- Rtsne::Rtsne(domain_mat, dims = 2, perplexity = min(30, nrow(domain_mat)/5))
    domain_2d <- domain_tsne$Y
    
  } else if (method == "umap") {
    # UMAP for causal features
    causal_umap <- umap::umap(causal_mat)
    causal_2d <- causal_umap$layout
    
    # UMAP for domain features
    domain_umap <- umap::umap(domain_mat)
    domain_2d <- domain_umap$layout
  }
  
  # Create data frames for plotting
  df_causal <- data.frame(
    x = causal_2d[, 1],
    y = causal_2d[, 2],
    label = as.factor(labels),
    type = "Causal Features (C)"
  )
  
  df_domain <- data.frame(
    x = domain_2d[, 1],
    y = domain_2d[, 2],
    label = as.factor(labels),
    type = "Domain Features (D)"
  )
  
  df_combined <- rbind(df_causal, df_domain)
  
  # Create plot
  p <- ggplot2::ggplot(df_combined, 
                       ggplot2::aes(x = x, y = y, color = label, shape = type)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      subtitle = paste("Dimensionality reduction:", toupper(method)),
      x = "Component 1",
      y = "Component 2",
      color = "Class",
      shape = "Feature Type"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::scale_shape_manual(values = c(16, 17))
  
  return(p)
}

#' Plot attention maps
#'
#' @param attention_weights Attention weights (matrix or 3D array)
#' @param image Original image (optional, matrix or array)
#' @param layer_name Name of the layer (for title)
#' @param threshold Threshold for highlighting (0-1)
#'
#' @return ggplot object or grid of plots
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient theme_minimal
#' @importFrom gridExtra grid.arrange
plot_attention_maps <- function(attention_weights, image = NULL, 
                                 layer_name = "Attention", threshold = 0.5) {
  
  # Convert to matrix if 3D array
  if (length(dim(attention_weights)) == 3) {
    # Average across channels
    attn_map <- apply(attention_weights, c(1, 2), mean)
  } else {
    attn_map <- as.matrix(attention_weights)
  }
  
  # Normalize to [0, 1]
  attn_map <- (attn_map - min(attn_map)) / (max(attn_map) - min(attn_map) + 1e-8)
  
  # Create data frame for ggplot
  df_attn <- expand.grid(
    x = 1:ncol(attn_map),
    y = 1:nrow(attn_map)
  )
  df_attn$weight <- as.vector(t(attn_map[nrow(attn_map):1, ]))  # Flip y for correct orientation
  
  # Create attention map plot
  p_attn <- ggplot2::ggplot(df_attn, ggplot2::aes(x = x, y = y, fill = weight)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(layer_name, "Map"),
      x = "Width",
      y = "Height",
      fill = "Attention"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  # If image is provided, also plot the image
  if (!is.null(image)) {
    # Convert image to data frame
    if (length(dim(image)) == 3) {
      # Color image - take first channel for simplicity
      img_gray <- image[, , 1]
    } else {
      img_gray <- as.matrix(image)
    }
    
    df_img <- expand.grid(
      x = 1:ncol(img_gray),
      y = 1:nrow(img_gray)
    )
    df_img$value <- as.vector(t(img_gray[nrow(img_gray):1, ]))
    
    p_img <- ggplot2::ggplot(df_img, ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "black", high = "white") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Original Image",
        x = "Width",
        y = "Height"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none"
      )
    
    # Create overlay (attention on image)
    df_overlay <- df_attn
    df_overlay$value <- df_img$value
    df_overlay$alpha <- df_attn$weight > threshold
    
    p_overlay <- ggplot2::ggplot(df_overlay, 
                                 ggplot2::aes(x = x, y = y, fill = value, alpha = alpha)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "black", high = "white") +
      ggplot2::scale_alpha_manual(values = c(0.3, 0.8)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste("Attention Overlay (threshold =", threshold, ")"),
        x = "Width",
        y = "Height"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none"
      )
    
    # Arrange plots in a grid
    gridExtra::grid.arrange(p_img, p_attn, p_overlay, ncol = 3)
  } else {
    # Just return attention map
    return(p_attn)
  }
}

#' Plot intervention strength analysis
#'
#' @param alpha_values Vector of alpha values tested
#' @param accuracy_values Matrix or data frame of accuracy values
#' @param tasks Names of transfer tasks
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal labs
plot_intervention_analysis <- function(alpha_values, accuracy_values, 
                                       tasks = c("Medical→Industrial", 
                                                "Industrial→Agricultural",
                                                "Agricultural→Security",
                                                "Security→Medical")) {
  
  # Convert to data frame
  if (is.matrix(accuracy_values)) {
    df <- as.data.frame(accuracy_values)
    colnames(df) <- tasks
  } else {
    df <- accuracy_values
  }
  
  df$alpha <- alpha_values
  
  # Reshape for ggplot
  df_long <- tidyr::pivot_longer(df, 
                                 cols = -alpha, 
                                 names_to = "task", 
                                 values_to = "accuracy")
  
  # Create plot
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = alpha, y = accuracy, 
                                             color = task, group = task)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 0.6, linetype = "dashed", color = "gray50", size = 0.8) +
    ggplot2::annotate("text", x = 0.62, y = max(df_long$accuracy) * 0.95, 
                     label = "α = 0.6 (optimal)", hjust = 0, size = 4) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Effect of Intervention Strength α on OOD Accuracy",
      x = "Intervention Strength α",
      y = "OOD Accuracy (%)",
      color = "Transfer Task"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(70, 90), labels = function(x) paste0(x, "%"))
  
  return(p)
}

#' Plot SHAP summary
#'
#' @param shap_values SHAP values from compute_shap_values
#' @param top_n Number of top features to show
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot theme_minimal labs coord_flip
plot_shap_summary <- function(shap_values, top_n = 10) {
  
  if (is.null(shap_values)) {
    stop("SHAP values are NULL")
  }
  
  # Compute mean absolute SHAP per feature
  mean_abs_shap <- colMeans(abs(shap_values))
  top_features <- order(mean_abs_shap, decreasing = TRUE)[1:min(top_n, length(mean_abs_shap))]
  
  # Prepare data for plotting
  df_list <- list()
  for (i in seq_along(top_features)) {
    feat_idx <- top_features[i]
    feat_name <- colnames(shap_values)[feat_idx]
    df_list[[i]] <- data.frame(
      feature = feat_name,
      value = shap_values[, feat_idx],
      importance = mean_abs_shap[feat_idx]
    )
  }
  df <- do.call(rbind, df_list)
  
  # Order features by importance
  df$feature <- factor(df$feature, 
                       levels = colnames(shap_values)[top_features[order(mean_abs_shap[top_features])]])
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = feature, y = value, fill = feature)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("SHAP Summary (Top", top_n, "Features)"),
      x = NULL,
      y = "SHAP Value"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    )
  
  return(p)
}

#' Plot training curves
#'
#' @param history Training history data frame with loss and metrics
#' @param metric Metric to plot ("loss", "accuracy", "f1")
#'
#' @return ggplot object
#' @export
plot_training_curves <- function(history, metric = c("loss", "accuracy", "f1")) {
  
  metric <- match.arg(metric)
  
  # Check if metric exists in history
  train_col <- paste0("train_", metric)
  val_col <- paste0("val_", metric)
  
  if (!train_col %in% colnames(history)) {
    stop(paste("Column", train_col, "not found in history"))
  }
  
  # Prepare data for plotting
  df <- data.frame(
    epoch = history$epoch,
    train = history[[train_col]],
    val = if (val_col %in% colnames(history)) history[[val_col]] else NA
  )
  
  # Reshape for ggplot
  df_long <- tidyr::pivot_longer(df, 
                                 cols = c(train, val), 
                                 names_to = "type", 
                                 values_to = "value")
  df_long <- df_long[!is.na(df_long$value), ]
  
  # Create plot
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = epoch, y = value, 
                                             color = type, group = type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Training", toupper(metric), "Curves"),
      x = "Epoch",
      y = toupper(metric),
      color = "Dataset"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    ) +
    ggplot2::scale_color_manual(values = c("train" = "blue", "val" = "red"),
                               labels = c("train" = "Training", "val" = "Validation"))
  
  return(p)
}

#' Plot confusion matrix
#'
#' @param cm Confusion matrix (table or matrix)
#' @param normalize Whether to normalize by row ("true"), column ("pred"), or none
#'
#' @return ggplot object
#' @export
plot_confusion_matrix <- function(cm, normalize = c("none", "true", "pred")) {
  
  normalize <- match.arg(normalize)
  
  # Convert to matrix if table
  if (is.table(cm)) {
    cm <- as.matrix(cm)
  }
  
  # Normalize if requested
  if (normalize == "true") {
    cm <- sweep(cm, 1, rowSums(cm), "/")
    fill_label <- "Proportion (True)"
  } else if (normalize == "pred") {
    cm <- sweep(cm, 2, colSums(cm), "/")
    fill_label <- "Proportion (Predicted)"
  } else {
    fill_label <- "Count"
  }
  
  # Prepare data for plotting
  df <- expand.grid(
    True = rownames(cm),
    Predicted = colnames(cm)
  )
  df$value <- as.vector(cm)
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Predicted, y = True, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3)), size = 4) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue", name = fill_label) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Confusion Matrix",
      x = "Predicted Label",
      y = "True Label"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}
