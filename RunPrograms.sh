for D in *; do
    if [ -d "${D}" ]; then
        echo "Problem number ${${D}:14"
        runhaskell "${D}/${D}.hs"
    fi
done