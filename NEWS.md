# ringbp 0.1.2.9999

* Fixed a bug in the implementation of quarantine, where isolation could happen later than onset + delay, if this was earlier than the isolation time of the infector. By @sbfnk in #107 and reviewed by @pearsonca and @joshwlambert.

# ringbp 0.1.2

* Improved package infrastructure by remove the number of dependencies, removing deprecated function calls and reducing clutter in the repo. In #70, #71, #72 and #73 by @sbfnk.

# ringbp 0.1.1

* Added unit tests. By @timcdlucas in #43 and reviewed by @sbfnk.

* Added testing and checking infrastructure using github actions.

# ringbp 0.1.0

* Initial release alongside paper on [Feasibility of controlling 2019-nCoV outbreaks by isolation of cases and contacts](https://doi.org/10.1016/S2214-109X(20)30074-7).
