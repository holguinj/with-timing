# with-timing

If you ever wonder:

  * "do I have time to get some coffee while these tests run?"
  * "is this command taking way longer than usual?"

Then with-timing might be for you. This small utility is a wrapper for long-running shell commands that:

  * records how long your commands usually take in a JSON-encoded file
  * on subsequent runs, uses the last recorded time to predict how long a command will take and when it will finish.

for example:

```
$ with-timing "sleep 2; echo done"
Unable to find a previous run.
done
Command executed successfully in 2 seconds.

$ with-timing "sleep 2; echo done"
The previous run finished in 2 seconds.
That suggests that this run will finish around 10:10:43 PM.
done
Command executed successfully in 2 seconds.
```

## installation

There are no packages or tarballs (yet), so you'll have to install it from source.
First, make sure you have [stack](https://haskellstack.org) installed, then:

1. `git clone https://github.com/juxtin/with-timing`
2. `cd with-timing`
3. `stack build && stack install`

If all goes well, you'll find `with-timing` in `~/.local/bin/with-timing`.

## flags

There are currently three supported flags, all of which are optional:

* **--file | -f**: the file where timing data is saved/retrieved. By default, it uses `~/.config/with-timing.json`.
* **--key | -k**: the JSON key under which to save/retrieve data for this run. Defaults to the full command string.
* **--allow-any-exitcode | -a**: record the timing for this run even if the command exits abnormally.

## example uses

### provisioning VMs takes forever

I have a script that gets provisions a set of VMs on my company's private infrastructure, but it can take 10-15 minutes or longer, depending on which flags I pass the provision script:

```
with-timing "provision.sh --fast-version"
```

`with-timing` uses the whole command as the key by default, so it will be sensitive to the differences in flags.
That means that running a quicker version of the command won't screw up the predictions for the longer-running version:

```
with-timing "provision.sh --slow-version"
```

However, if other flags don't make much of a difference to the overall time, you should specify a custom `--key` argument:

```
with-timing --key "provision --slow-version" "provision.sh --slow-version --force --quiet"
```

The `--key` argument can be any string; it doesn't have to actually look like the command.
The following will also work:

```
with-timing --key "provision (slow)" "provision.sh --slow-version --verbose"
```

### these tests take forever

I work on a lot of Clojure projects where the tests take 5-10 minutes (or longer) to run, so I have this alias defined:

`alias lta='with-timing --key "$(pwd):tests" "lein test :all"'`

It's important to include `$(pwd)` in the `--key` argument because the command `lein test :all` will take more or less time depending on the project I'm testing.
I encourage you to use interpolation like that to make sure your predictions are useful.

Another thing to keep in mind with this use case: by default, if the command exits non-zero (e.g., if test fail) then the timing won't be recorded for next time.
To change this behavior, use the `--allow-any-exitcode` flag.

### some computers are faster than others

If you regularly work on a few different systems, you may find that some of your commands are highly dependent which system you're currently on (unit tests), while others are limited mostly by external factors (provisioning cloud VMs).

If you're synchronizing dotfiles across your computers, you may want to synchronize only with-timing data that won't vary too much.
That's where the `--file` argument can really help. Check out these updated versions of the above commands:

```
alias lta='with-timing            --file "~/.config/with-timing-local.json"      --key "$(pwd):test"      "lein test :all"'
alias provision-slow='with-timing --file "~/.dotfiles/with-timing-external.json" --key "provision (slow)" "provision.sh --slow-version"'
```

Just synchronize `~/.dotfiles/with-timing-external.json` however you like, and enjoy accurate(ish) predictions wherever you go.
