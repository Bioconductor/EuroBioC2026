countdown_timer <- function(target_datetime,
                            label = "Conference starts in",
                            end_message = "The conference has started 🎉") {
    target <- format(as.POSIXct(target_datetime), "%Y-%m-%dT%H:%M:%S")
    id <- paste0("countdown-", as.integer(Sys.time()))

    html <- sprintf(
        '
<div id="%s" class="countdown-box">
  <div class="countdown-label">%s</div>
  <div class="countdown-time">Loading...</div>
</div>

<style>
  .countdown-box {
    text-align: center;
    border: 1px solid #e6e6e6;
    border-radius: 14px;
    padding: 18px 12px;
    margin: 18px 0;
    background: #ffffff;
    box-shadow: 0 2px 10px rgba(0,0,0,0.04);
  }

  .countdown-label {
    font-size: 1.1em;
    color: #2c3e50;
    margin-bottom: 6px;
    font-weight: 600;
  }

  .countdown-time {
    font-size: 1.9em;
    font-weight: 700;
    letter-spacing: 1px;
    color: #2c3e50;
  }

  .countdown-done {
    font-size: 1.6em;
    font-weight: 700;
    color: #2c3e50;
  }
</style>

<script>
document.addEventListener("DOMContentLoaded", function() {

  const targetDate = new Date("%s").getTime();
  const el = document.querySelector("#%s .countdown-time");

  function updateCountdown() {
    const now = new Date().getTime();
    const diff = targetDate - now;

    if (!el) return;

    if (diff <= 0) {
      el.className = "countdown-done";
      el.innerHTML = "%s";
      clearInterval(timer);
      return;
    }

    const d = Math.floor(diff / (1000 * 60 * 60 * 24));
    const h = Math.floor((diff / (1000 * 60 * 60)) %% 24);
    const m = Math.floor((diff / (1000 * 60)) %% 60);
    const s = Math.floor((diff / 1000) %% 60);

    el.innerHTML = d + "d " + h + "h " + m + "m " + s + "s";
  }

  updateCountdown();
  const timer = setInterval(updateCountdown, 1000);

});
</script>
',
        id, label,
        target, id, end_message
    )

    cat(html)
}
