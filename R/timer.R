
countdown_timer <- function(start_datetime,
                            end_datetime,
                            label = "Conference starts in",
                            start_message = "The conference has started 🎉",
                            end_message = "The conference has ended. <i>Kiitos</i> for joining us! ❤️") {

  start_target <- format(as.POSIXct(start_datetime), "%Y-%m-%dT%H:%M:%S")
  end_target <- format(as.POSIXct(end_datetime), "%Y-%m-%dT%H:%M:%S")

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

  const startDate = new Date("%s").getTime();
  const endDate = new Date("%s").getTime();

  const el = document.querySelector("#%s .countdown-time");
  const labelEl = document.querySelector("#%s .countdown-label");

  function updateCountdown() {

    const now = new Date().getTime();

    if (!el) return;

    // BEFORE conference
    if (now < startDate) {

      labelEl.style.display = "block";

      const diff = startDate - now;

      const d = Math.floor(diff / (1000 * 60 * 60 * 24));
      const h = Math.floor((diff / (1000 * 60 * 60)) %% 24);
      const m = Math.floor((diff / (1000 * 60)) %% 60);
      const s = Math.floor((diff / 1000) %% 60);

      el.className = "countdown-time";
      el.innerHTML = d + "d " + h + "h " + m + "m " + s + "s";
    }

    // DURING conference
    else if (now >= startDate && now <= endDate) {

      labelEl.style.display = "none";

      el.className = "countdown-done";
      el.innerHTML = "%s";
    }

    // AFTER conference
    else {

      labelEl.style.display = "none";

      el.className = "countdown-done";
      el.innerHTML = "%s";

      clearInterval(timer);
    }
  }

  updateCountdown();
  const timer = setInterval(updateCountdown, 1000);

});
</script>
',
    id,
    label,
    start_target,
    end_target,
    id,
    id,
    start_message,
    end_message
  )

  cat(html)
}
