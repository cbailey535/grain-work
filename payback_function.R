rm(list = ls())

# Input values
bulk_arpu <- 35.00
take_percent <- 0.15
margin <- 0.65
cost_per_unit <- 2000.00
cost_per_foot <- 17.00
type_ii_cost_per_month <- 2000.00
num_units <- 250
distance_adj <- 1.2
network_prox <- 2500
bulk_arpu_growth <- 1.04
take_rev_growth <-  0.90
type_ii_cost_growth <- 1.03

offnet_payback <- function(bulk_arpu, take_percent, margin, cost_per_unit, type_ii_cost_per_month, num_units, years = 10) {
  # Initialize variables for Year 1
  initial_bulk_arpu <- bulk_arpu
  bulk_revenue <- bulk_arpu * 12 * num_units
  take_revenue <- bulk_revenue * take_percent
  annual_revenue <- bulk_revenue + take_revenue
  
  direct_cost <- annual_revenue * -(1 - margin)
  type_ii_cost <- type_ii_cost_per_month * 12
  capex <- cost_per_unit * num_units
  
  # Initialize cash flow and cumulative cash flow
  cash_flows <- numeric(years + 1)
  cumulative_cf <- numeric(years + 1)
  
  # Period 0: Initial Capex (do not include in payback period)
  cash_flows[1] <- -capex
  cumulative_cf[1] <- cash_flows[1]
  
  # Loop through each year to calculate cash flow, cumulative cash flow, and payback period
  for (i in 2:(years + 1)) {
    # Apply annual growth rates starting from Year 2
    if (i > 2) {
      bulk_arpu <- bulk_arpu * bulk_arpu_growth  # Increase bulk ARPU by 4% annually
      bulk_revenue <- bulk_arpu * 12 * num_units  # Recalculate bulk revenue using updated bulk_arpu
      take_revenue <- take_revenue * take_rev_growth  # Decrease take revenue by 10% annually
      type_ii_cost <- type_ii_cost * type_ii_cost_growth  # Increase type II cost by 3% annually
    }
    
    # Recalculate annual revenue and direct cost for the current year
    annual_revenue <- bulk_revenue + take_revenue
    direct_cost <- annual_revenue * -(1 - margin)
    
    # Contribution margin calculation
    contribution_margin <- annual_revenue - abs(direct_cost) - type_ii_cost
    
    # Calculate cash flows and update cumulative cash flow
    cash_flows[i] <- contribution_margin
    cumulative_cf[i] <- cumulative_cf[i - 1] + cash_flows[i]
    
    # Debugging: Track each cash flow and cumulative cash flow
    #print(paste("Year", i - 1, "Bulk Revenue:", bulk_revenue))
    #print(paste("Year", i - 1, "Take Revenue:", take_revenue))
    #print(paste("Year", i - 1, "Annual Revenue:", annual_revenue))
    #print(paste("Year", i - 1, "Direct Cost:", direct_cost))
    #print(paste("Year", i - 1, "Type II Cost:", type_ii_cost))
    #print(paste("Year", i - 1, "Contribution Margin:", contribution_margin))
    #print(paste("Year", i - 1, "Cash Flow:", cash_flows[i]))
    #print(paste("Year", i - 1, "Cumulative Cash Flow:", cumulative_cf[i]))
  }
  
  # Determine the first year where cumulative cash flow becomes positive (excluding Year 0)
  first_positive_year <- which(cumulative_cf >= 0)[1] - 1
  
  if (is.na(first_positive_year)) { # Handle case where payback is not achieved
    return(list(payback_period = NA, cumulative_cash_flow = cumulative_cf))
  }
  
  # Calculate the fractional year based on remaining cumulative CF in the first positive year
  remaining <- abs(cumulative_cf[first_positive_year])  # Remaining cumulative CF before full payback
  next_year_cf <- cash_flows[first_positive_year + 1]  # Cash flow in the next period
  fractional_year <- remaining / next_year_cf  # Calculate the fractional year
  
  # Final payback period calculation, excluding Year 0
  payback_period <- (first_positive_year + fractional_year) - 1
  
  # Return result
  return(list(payback_period = payback_period, cumulative_cash_flow = cumulative_cf))
}


# Calculate payback
offnet_result <- offnet_payback(bulk_arpu, take_percent, margin, cost_per_unit, type_ii_cost_per_month, num_units)

# Output results
cat("Payback Period:", offnet_result$payback_period, "years\n")
cat("Cumulative Cash Flow:\n")
print(offnet_result$cumulative_cash_flow)

################################################################################
################################################################################

onnet_payback <- function(bulk_arpu, take_percent, margin, cost_per_unit, cost_per_foot, num_units, distance_adj, network_prox, years = 10) {
  # Initialize variables for Year 1
  initial_bulk_arpu <- bulk_arpu
  bulk_revenue <- bulk_arpu * 12 * num_units
  take_revenue <- bulk_revenue * take_percent
  annual_revenue <- bulk_revenue + take_revenue
  
  direct_cost <- annual_revenue * -(1 - margin)
  non_transport <- num_units * cost_per_unit
  transport <- network_prox * cost_per_foot * distance_adj
  capex <- non_transport + transport
  
  # Initialize cash flow and cumulative cash flow
  cash_flows <- numeric(years + 1)
  cumulative_cf <- numeric(years + 1)
  
  # Period 0: Initial Capex (do not include in payback period)
  cash_flows[1] <- -capex
  cumulative_cf[1] <- cash_flows[1]
  
  # Loop through each year to calculate cash flow, cumulative cash flow, and payback period
  for (i in 2:(years + 1)) {
    # Apply annual growth rates starting from Year 2
    if (i > 2) {
      bulk_arpu <- bulk_arpu * bulk_arpu_growth  # Increase bulk ARPU by 4% annually
      bulk_revenue <- bulk_arpu * 12 * num_units  # Recalculate bulk revenue using updated bulk_arpu
      take_revenue <- take_revenue * take_rev_growth  # Decrease take revenue by 10% annually
    }
    
    # Recalculate annual revenue and direct cost for the current year
    annual_revenue <- bulk_revenue + take_revenue
    direct_cost <- annual_revenue * -(1 - margin)
    
    # Contribution margin calculation
    contribution_margin <- annual_revenue - abs(direct_cost)
    
    # Calculate cash flows and update cumulative cash flow
    cash_flows[i] <- contribution_margin
    cumulative_cf[i] <- cumulative_cf[i - 1] + cash_flows[i]
    
    # Debugging: Track each cash flow and cumulative cash flow
    #print(paste("Year", i - 1, "Bulk Revenue:", bulk_revenue))
    #print(paste("Year", i - 1, "Take Revenue:", take_revenue))
    #print(paste("Year", i - 1, "Annual Revenue:", annual_revenue))
    #print(paste("Year", i - 1, "Direct Cost:", direct_cost))
    #print(paste("Year", i - 1, "Cash Flow:", cash_flows[i]))
    #print(paste("Year", i - 1, "Cumulative Cash Flow:", cumulative_cf[i]))
  }
  
  # Determine the first year where cumulative cash flow becomes positive (excluding Year 0)
  first_positive_year <- which(cumulative_cf >= 0)[1] - 1
  
  if (is.na(first_positive_year)) { # Handle case where payback is not achieved
    return(list(payback_period = NA, cumulative_cash_flow = cumulative_cf))
  }
  
  # Calculate the fractional year based on remaining cumulative CF in the first positive year
  remaining <- abs(cumulative_cf[first_positive_year])  # Remaining cumulative CF before full payback
  next_year_cf <- cash_flows[first_positive_year + 1]  # Cash flow in the next period
  fractional_year <- remaining / next_year_cf  # Calculate the fractional year
  
  # Final payback period calculation, excluding Year 0
  payback_period <- (first_positive_year + fractional_year) - 1
  
  # Return result
  return(list(payback_period = payback_period, cumulative_cash_flow = cumulative_cf))
}

# Calculate payback
onnet_result <- onnet_payback(bulk_arpu, take_percent, margin, cost_per_unit, cost_per_foot, num_units, distance_adj, network_prox)

# Output results
cat("Payback Period:", onnet_result$payback_period, "years\n")
cat("Cumulative Cash Flow:\n")
print(onnet_result$cumulative_cash_flow)
