#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix normalize_quantiles_dense(NumericMatrix x, bool copy = true, bool keep_names = false) {
    int rows = x.nrow();
    int cols = x.ncol();
    
    NumericMatrix mat;
    if (copy) {
        mat = clone(x);
    } else {
        mat = x;
    }
    
    CharacterVector row_names, col_names;
    if (keep_names) {
        row_names = rownames(x);
        col_names = colnames(x);
    }
    
    // Track NA positions
    LogicalMatrix na_pos(rows, cols);
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            na_pos(i, j) = NumericVector::is_na(mat(i, j));
        }
    }
    
    // Sort each column
    std::vector<std::vector<double>> sorted_cols(cols);
    
    for (int j = 0; j < cols; j++) {
        std::vector<double> col_data;
        col_data.reserve(rows);
        
        for (int i = 0; i < rows; i++) {
            if (!na_pos(i, j)) {
                col_data.push_back(mat(i, j));
            }
        }
        
        std::sort(col_data.begin(), col_data.end());
        sorted_cols[j] = col_data;
    }
    
    // Compute target distribution
    int max_valid = 0;
    for (int j = 0; j < cols; j++) {
        max_valid = std::max(max_valid, (int)sorted_cols[j].size());
    }
    
    std::vector<double> target_dist(max_valid, 0.0);
    std::vector<int> counts(max_valid, 0);
    
    for (int j = 0; j < cols; j++) {
        for (size_t i = 0; i < sorted_cols[j].size(); i++) {
            target_dist[i] += sorted_cols[j][i];
            counts[i]++;
        }
    }
    
    for (int i = 0; i < max_valid; i++) {
        if (counts[i] > 0) {
            target_dist[i] /= counts[i];
        }
    }
    
    // Assign normalized values
    for (int j = 0; j < cols; j++) {
        std::vector<std::pair<double, int>> val_idx;
        val_idx.reserve(rows);
        
        for (int i = 0; i < rows; i++) {
            if (!na_pos(i, j)) {
                val_idx.push_back({mat(i, j), i});
            }
        }
        
        std::sort(val_idx.begin(), val_idx.end());
        
        for (size_t r = 0; r < val_idx.size(); r++) {
            int orig_idx = val_idx[r].second;
            mat(orig_idx, j) = target_dist[r];
        }
        
        for (int i = 0; i < rows; i++) {
            if (na_pos(i, j)) {
                mat(i, j) = NA_REAL;
            }
        }
    }
    
    if (keep_names) {
        if (row_names.size() > 0) rownames(mat) = row_names;
        if (col_names.size() > 0) colnames(mat) = col_names;
    }
    
    return mat;
}

// [[Rcpp::export]]
SEXP normalize_quantiles_sparse(SEXP x, bool copy = true, bool keep_names = false) {
    // Get dimensions
    IntegerVector dims = Rf_getAttrib(x, wrap("Dim"));
    int rows = dims[0];
    int cols = dims[1];
    
    // Get sparse matrix components (dgCMatrix format)
    NumericVector x_vals = Rf_getAttrib(x, wrap("x"));
    IntegerVector i_indices = Rf_getAttrib(x, wrap("i"));  // row indices
    IntegerVector p_pointers = Rf_getAttrib(x, wrap("p")); // column pointers
    
    // Store names
    List dimnames_list;
    if (keep_names) {
        dimnames_list = Rf_getAttrib(x, wrap("Dimnames"));
    }
    
    // Extract values for each column
    std::vector<std::vector<std::pair<double, int>>> col_values(cols);
    
    for (int j = 0; j < cols; j++) {
        int start = p_pointers[j];
        int end = p_pointers[j + 1];
        
        // Collect non-zero values with their row indices
        for (int idx = start; idx < end; idx++) {
            int row_idx = i_indices[idx];
            double val = x_vals[idx];
            if (!NumericVector::is_na(val)) {
                col_values[j].push_back({val, row_idx});
            }
        }
        
        // Add zeros for remaining rows
        std::vector<bool> has_value(rows, false);
        for (int idx = start; idx < end; idx++) {
            has_value[i_indices[idx]] = true;
        }
        for (int i = 0; i < rows; i++) {
            if (!has_value[i]) {
                col_values[j].push_back({0.0, i});
            }
        }
    }
    
    // Sort each column
    std::vector<std::vector<double>> sorted_cols(cols);
    for (int j = 0; j < cols; j++) {
        std::vector<double> col_sorted;
        col_sorted.reserve(col_values[j].size());
        for (auto& pair : col_values[j]) {
            col_sorted.push_back(pair.first);
        }
        std::sort(col_sorted.begin(), col_sorted.end());
        sorted_cols[j] = col_sorted;
    }
    
    // Compute target distribution
    std::vector<double> target_dist(rows, 0.0);
    for (int i = 0; i < rows; i++) {
        double sum = 0.0;
        int count = 0;
        for (int j = 0; j < cols; j++) {
            if (i < (int)sorted_cols[j].size()) {
                sum += sorted_cols[j][i];
                count++;
            }
        }
        target_dist[i] = (count > 0) ? sum / count : 0.0;
    }
    
    // Build normalized sparse matrix
    std::vector<double> new_x;
    std::vector<int> new_i;
    std::vector<int> new_p(cols + 1, 0);
    
    for (int j = 0; j < cols; j++) {
        // Sort by value to get ranks
        std::sort(col_values[j].begin(), col_values[j].end());
        
        // Assign normalized values
        std::vector<std::pair<int, double>> row_val_pairs;
        for (size_t r = 0; r < col_values[j].size(); r++) {
            int row_idx = col_values[j][r].second;
            double norm_val = target_dist[r];
            if (norm_val != 0.0) {  // Only store non-zeros in sparse format
                row_val_pairs.push_back({row_idx, norm_val});
            }
        }
        
        // Sort by row index for proper sparse format
        std::sort(row_val_pairs.begin(), row_val_pairs.end());
        
        new_p[j + 1] = new_p[j] + row_val_pairs.size();
        for (auto& pair : row_val_pairs) {
            new_i.push_back(pair.first);
            new_x.push_back(pair.second);
        }
    }
    
    // Create new dgCMatrix
    S4 result("dgCMatrix");
    result.slot("i") = wrap(new_i);
    result.slot("p") = wrap(new_p);
    result.slot("x") = wrap(new_x);
    result.slot("Dim") = dims;
    
    if (keep_names && !Rf_isNull(dimnames_list)) {
        result.slot("Dimnames") = dimnames_list;
    }
    
    return result;
}

// [[Rcpp::export]]
SEXP normalize_quantiles_cpp(SEXP x, bool copy = true, bool keep_names = false) {
    // Check if input is a sparse matrix (dgCMatrix, dgRMatrix, etc.)
    bool is_sparse = false;
    std::string class_name = "";
    
    if (Rf_isS4(x)) {
        CharacterVector classes = Rf_getAttrib(x, R_ClassSymbol);
        if (classes.size() > 0) {
            class_name = as<std::string>(classes[0]);
            is_sparse = (class_name.find("Matrix") != std::string::npos);
        }
    }
    
    if (is_sparse) {
        return normalize_quantiles_sparse(x, copy, keep_names);
    } else {
        return normalize_quantiles_dense(wrap(x), copy, keep_names);
    }
}
