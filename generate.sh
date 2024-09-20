#!/usr/bin/env bash

srcdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )

get_yaml() {
    # Arg 1: the meta.yml path
    # STDIN: the mustache code
    local meta="$1" temp
    temp=$(mktemp template-XXX)
    cat > "$temp"
    mustache "$meta" "$temp"
    rm -f -- "$temp"
}

for f in "$srcdir"/{,.}*.mustache; do
    target=$(basename "$f" .mustache)
    case "$target" in
        coq.opam)
            mustache='{{ opam_name }}'
            opam_name=$(get_yaml meta.yml <<<"$mustache")
	    if [ -n "$opam_name" ]; then
		target=${target/coq/$opam_name}
	    else
		mustache='{{ shortname }}'
		shortname=$(get_yaml meta.yml <<<"$mustache")
		if [ -n "$shortname" ]; then
		    target=${target/coq/coq-$shortname}
	        else
		    continue
		fi
            fi
            ;;
        extracted.opam)
            mustache='{{# extracted }}{{ extracted_shortname }}{{/ extracted }}'
            extracted_shortname=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$extracted_shortname" ]; then
                target=${target/extracted/$extracted_shortname}
            else
                continue
            fi
            ;;
        dune-project)
            mustache='{{ dune }}'
            bool=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$bool" ] && [ "$bool" != false ]; then
                : noop
            else
                continue
            fi
            ;;
        dune)
            mustache='{{ dune }}'
            bool=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$bool" ] && [ "$bool" != false ]; then
                mkdir -p -v theories && target="theories/$target"
            else
                continue
            fi
            ;;
        index.md)
            mustache='{{ coqdoc }}'
            bool=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$bool" ] && [ "$bool" != false ]; then
                mkdir -p -v resources && target="resources/$target"
            else
                continue
            fi
            ;;
        docker-action.yml)
            mustache='{{ action }}'
            bool=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$bool" ] && [ "$bool" != false ]; then
                mkdir -p -v .github/workflows && target=".github/workflows/$target"
            else
                continue
            fi
            ;;
        nix-action.yml)
            action_mustache='{{ action }}'
            action_bool=$(get_yaml meta.yml <<<"$action_mustache")
            nix_mustache='{{ nix }}'
            nix_bool=$(get_yaml meta.yml <<<"$nix_mustache")
            if [ -n "$action_bool" ] && [ -n "$nix_bool" ] && [ "$nix_bool" != false ] && [ "$action_bool" != false ]; then
                mkdir -p -v .github/workflows && target=".github/workflows/$target"
            else
                continue
            fi
            ;;
        .travis.yml)
            mustache='{{ travis }}'
            bool=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$bool" ] && [ "$bool" != false ]; then
                : noop
            else
                continue
            fi
            ;;
	config.yml)
	    mustache='{{ circleci }}'
	    bool=$(get_yaml meta.yml <<<"$mustache")
            if [ -n "$bool" ] && [ "$bool" != false ]; then
                mkdir -p -v .circleci && target=".circleci/$target"
            else
                continue
            fi
            ;;
    esac
    listed=false
    for specified_target in "$@"; do
        if [ "$specified_target" == "$target" ]; then
            listed=true
	fi
    done
    if [ $# -gt 0 ] && [ $listed != true ]; then
	continue
    fi
    echo "Generating $target..."
    mustache meta.yml "$f" > "$target"
done
