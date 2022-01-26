#!/bin/bash 
# Format barcodes 
BC=$(
  gawk \
    -v BR="257-282" \
    -v BP="barcode" \
    ' 
      # Define function for reverse complement
      function revcomp(s,  i, o) {
        o = ""
        for(i = length; i > 0; i--)
             o = o c[substr(s, i, 1)]
        return(o)
      }
      BEGIN{
        # Define revcomp mapping vector
        c["A"] = "T"
        c["C"] = "G"
        c["G"] = "C"
        c["T"] = "A"
        
        # Format barcode ranges for testing
        split(BR, RANGES, ";")
        for (R in RANGES){
          FW = RANGES[R]
          sub("-.*", "", FW)
          RV = RANGES[R]
          sub(".*-", "", RV)
          FW_L[++j]=FW+0
          RV_L[j]=RV+0
        }
      }
      # Store barcodes matching ranges
      NR>=2{
        BNR=$1
        sub(BP, "", BNR)
        for (R in FW_L){
          if ( BNR+0 >= FW_L[R] && BNR+0 <= RV_L[R]){
            FW_SEQ=$3 $4
            RV_SEQ=revcomp($6 $7)
            FW_SEQ_RC=revcomp($3 $4)
            RV_SEQ_RC=$6 $7
            print ">" $1 "\n" FW_SEQ "..." RV_SEQ
            print ">" $1 "\n" RV_SEQ_RC "..." FW_SEQ_RC  
          }
        }
      }
    ' barcodes.tsv
)

echo "$BC" > barcodes_used.fa