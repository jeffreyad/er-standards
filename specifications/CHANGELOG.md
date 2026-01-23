
> cat("# Specification Change Log\n\n")
# Specification Change Log


> cat("## Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
## Date: 2026-01-23 14:56:56 


> cat("## Variable Name Changes\n\n")
## Variable Name Changes


> cat("### Common Changes (All Datasets)\n\n")
### Common Changes (All Datasets)


> cat("| Old Variable | New Variable | Reason |\n")
| Old Variable | New Variable | Reason |

> cat("|--------------|--------------|--------|\n")
|--------------|--------------|--------|

> for (i in 1:nrow(common_exposure_changes)) {
+   cat(sprintf("| %s | %s | %s |\n",
+               common_exposure_changes$Old_Variable[i],
+        .... [TRUNCATED] 
| AUCSSLOG | AUCSLOG | 8-char limit: removed one S |
| CMAXSSLOG | CMXSLOG | 8-char limit: abbreviated CMAX |
| CAVGSSLOG | CAVGLOG | 8-char limit: removed SS |
| CMAXSSSTD | CMXSSSTD | 8-char limit: abbreviated CMAX |
| AUCSSDOSE | AUCSSDOS | 8-char limit: removed E |
| CMAXSSDOSE | CMXSSDOS | 8-char limit: abbreviated CMAX + removed E |
| AUCSSCATN | AUCSCATN | 8-char limit: removed one S |

> cat("\n### ADES-Specific Changes\n\n")

### ADES-Specific Changes


> cat("| Old Variable | New Variable | Reason |\n")
| Old Variable | New Variable | Reason |

> cat("|--------------|--------------|--------|\n")
|--------------|--------------|--------|

> for (i in 1:nrow(ades_changes)) {
+   cat(sprintf("| %s | %s | %s |\n",
+               ades_changes$Old_Variable[i],
+               ades_changes$N .... [TRUNCATED] 
| TEAEGR3 | TEAESEV | Changed from grade to severity |
| AETOXGR | AESEV | pharmaverseadam uses ASEV not AETOXGR |
| AETOXGRN | AESEVN | pharmaverseadam uses ASEVN not AETOXGRN |

> cat("\n### ADTRR-Specific Changes\n\n")

### ADTRR-Specific Changes


> cat("| Old Variable | New Variable | Reason |\n")
| Old Variable | New Variable | Reason |

> cat("|--------------|--------------|--------|\n")
|--------------|--------------|--------|

> for (i in 1:nrow(adtrr_changes)) {
+   cat(sprintf("| %s | %s | %s |\n",
+               adtrr_changes$Old_Variable[i],
+               adtrr_change .... [TRUNCATED] 
| NADIR_PCHG | NADPCHG | 8-char limit |
| NADIR_VISIT | NADVST | 8-char limit |

> cat("\n## New Variables Added\n\n")

## New Variables Added


> cat("### ADES\n\n")
### ADES


> cat("- AERELN: Numeric version of AEREL\n\n")
- AERELN: Numeric version of AEREL


> cat("### ADTRR\n\n")
### ADTRR


> cat("- BORN: Numeric version of BOR\n")
- BORN: Numeric version of BOR

> cat("- AVALN: Numeric analysis value\n")
- AVALN: Numeric analysis value

> cat("- NADIR: Nadir tumor size value\n")
- NADIR: Nadir tumor size value

> sink()
