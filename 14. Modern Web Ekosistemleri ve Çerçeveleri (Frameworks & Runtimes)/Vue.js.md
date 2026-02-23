# Vue.js

## Özet
Vue.js (Viuu/View olarak okunur); 2014 yılında Eski Google Mühendisi Evan You tarafından (Angular'ın dev karmaşıklığından nefret edip) "Daha Hafif, Daha Basit Gelişebilen ve İhtiyaç oldukça genişleyen" bir Front-End (Ön Yüz) reaktif arayüz kütüphanesi/çerçevesi yapmak vizyonuyla İcat(Fork) edilen, **Dünyanın En Çok Sevilen (En Az Nefret Edilen) ve Geliştirme Deneyimi(DX) Zirvesinde olan** devasa Açık-Kaynak Front-End Tek-Sayfa(SPA) Framwork'üdür.

## Nedir ve Ne İşe Yarar?
2014 yıllarında Piyasada İki Devasa Krallık Web Savaşındaydı: 
1. **Google'ın Angular'ı:** Devasa bir Şato gibiydi, C# (Typescript) kuralları vardı, çok öğrenmesi zor (Dik Öğrenme Eğrisi) ve kurması ağlatıyordu.
2. **Meta(Facebook)'ın React'ı:** Arkasında Trilyon Dolarlık şirket olan muazzam bir Kütüphaneydi. Fakat Mimarisi Çok Zordu. (JS içine HTML(JSX) yazmak, State'leri Karışık yönetmek).

Evan You dedi ki: "Angular'ın o Html Sayfalara veri yapıştırma güzelliğini (Veri Bağlayıcılığı) alsam... React'ın ise Sanal İskelete (Virtual DOM) hızlıca boyama zarafetini Cımbızlasam... Ve Bütün bunların Tasarımını/Kurulumunu "Mükemmel Anlaşılır ÜÇ-PARÇAYA Bölsem": (Buna **.vue Dosyası - Single File Component** denildi). Ve VUE.js Böylece patladı. Çin piyasasının (Alibaba vb) ve Laravel(PHP) ekosisteminin Varsayılan(Default) ön-yüz Dini/Motoru haline geldi.

**Ne İşe Yarar?**
* Aynı React Gibi Dinamik, Yenilenmeden Sayfayı Değiştiren İnanılmaz Yüksek Performanslı Web Ekosistemleri Ekrana çizer.
* Progresive Framework (Aşamalı): Bir projeye Vue Eklemek için React'taki gibi Bütün Kod mimarisini Sıfırdan Yıkmanıza gerek Yoktur. Dümdüz Bir `<script>` tagı (Eskiden HTML kafaına) olarak sayfaya zımbalayıp, "Sadece şu Div/kutu bölgesine Reaktif Animasyon kat" diyebilmesini inanılmaz derecede "Aşamalı/Barışçıl" eklenti formatında yapar.

## Dilin Mantığı ve Kod Yapısı
Vue.js'in React'a Asla Benzeyemeyen (ve Milyonlarca Web Geliştiricisini Klasik döneme aşık eden) Temel Zekası: **.vue (Single File Component - SFC)** dosyasıdır. Vue dedi ki "React herşeyi karıştırıyor. Bizim tek dosyamız olacak ama 3 Net Bloktan oluşacak":

1. `<template>`: Klasik Dümdüz HTML'ini Buraya yaz! (İçine If/Else için v-if, Döngüler için v-for yazılır ki Angular Klonudur).
2. `<script setup>`: Saf Javascript veya Typesriptini Buraya Yaz! Değişken Tutar (State).
3. `<style>`: Tasarım/CSS Kısmını buraya YaZ! (Sadece bu Componentslere etki et - Scoped CSS!).

Yani Web'in o klasik (Kutsal Üçlü - HTML,CSS,JS) Evrensel tasarımından Asla kopmadığı için çok net öğrenilir ve çok daha az Kod satırı (Boilerplate) ile harikalar yaratır.

**Örnek İşleyiş (Sembolik Olarak):**
Değişkeni HTML'de Ekrsana Bastırma: `<h1> Merhaba O Gezgin, {{kullanici.isimi}}! </h1>` (Çift Süslü Parantez Mucizesi - Bıyık/Mustache Interpolasyonu). 
Tıklanma Zekası (Direktifler) : `@click="GirisYap"` veya `v-on:click`

### Örnek Bir Vue.js (v3 - Composition API / Setup) Kodu: Girdi Kutularını Anından Listeye Ekleyen To-Do
Çok kısa ve Düzenli olan o Üç-Katmanlı mimarinin React'ın "return()" karmaşasına Çektiği Cila: Tıklanıldıkça Aşağıya görev Ekleme(Senkronizasyon)

```html
<!-- BİR VUE DOSYASI (.vue) YAPISI -->

<!-- 1. JAVASCRIPT/STATE MİMARİSİ (Composition API) -->
<!-- 'setup' tagi Vue3 un Devrimidir: Component/Fonksiyon kurmadan Direkt duz daktilo JS'sini aninda Derler-->
<script setup>
        // Vue'dan reaktif(canli) degisken yaratan Kancayi(Ref) cek:
        import { ref } from 'vue'

        // Data / Stateler (Sanal DOM'un haberdar edilcegi İçi Tıklanınca şisen Degişkenler):
        // 1. Yazacagimiz yeni metni tutacak String
        const yeniGorevMesaji = ref('') 
        // 2. Gorevleri tutacak Dizi (Array) Başta 'Vue Ogren' Var!
        const yapilacaklarListesi = ref([ { id: 1, metin: 'Vue Ögrencek ve Krallık kuracan' } ])

        // Islem Gorgen Fonksiyon: (Ekle Butonuna Basildiginda:)
        function listeyeEkletOglum() {
            // Eger kullanici bosluk girmedisye:
            if (yeniGorevMesaji.value.trim() !== '') { 
                // Yeni Array Satiri (Push/Listeye tak!):
                yapilacaklarListesi.value.push({
                    id: Date.now(),
                    metin: yeniGorevMesaji.value 
                });
                
                // İsledikten Sonra Input Kutusu(Htmlsini) Sifirla(Bosalt!):
                yeniGorevMesaji.value = '';
            }
        }
</script>

<!-- ============================================== -->

<!-- 2. HTML SABLONU (KULLANICININ GORSELİ) -->
<!-- Reacttaki Igrenc JSX yerine SAPSADE ve Temiz bir HTML Tag mimarisi: -->
<template>
  <div class="kutu-tasarimi">

    <!-- IKI YONLU VERI BAGLAMA (v-model MUCİZESİ):  -->
    <!-- V-Model sunu der: Aşağidaki Metin Kutusuna Adam ne daktillo(Yazıyorsa),  -->
    <!-- Üsteki JS'deki Const (yeniGorevMesaji) Degikseniye aninda(Milisaniye) ESLE (Sync)! -->
    <input 
      v-model="yeniGorevMesaji" 
      @keyup.enter="listeyeEkletOglum"  
      placeholder="Ne Edecegiz Usta!?" 
    />
    
    <!-- @click (V-ON Mımarısı) Sayesinde JavaScript fonskiyonunu tetikleme -->
    <button @click="listeyeEkletOglum">Listeye Firlat!</button>

    <ul>
      <!-- V-FOR DONGUSU(Directives): React'daki O İğrenc "Array.Map" eziyetini Kaldırır Cümle Vurur -->
      <!-- 'yapilacaklarListesi' inde Kac Tane eleman (Satır) Varsa O Kadar <li> Htmlyi Sİşir ekrana ciz" -->
      <li v-for="gorev in yapilacaklarListesi" :key="gorev.id">
         {{ gorev.metin }}  <!-- Biyikli/Mustache Metin enjeksiyonu -->
      </li>
    </ul>

  </div>
</template>

<!-- ============================================== -->

<!-- 3. TASARIM / MAKYAJ STili (CSS) -->
<!-- "scoped" Yazarsaniz bu CSS Asla diğer Componentlerin (Header vs) Tasarımina SIZMAZ ve Bozmaz! -->
<style scoped>
  input { border: 2px solid green; padding: 10px; }
  button { background-color: darkgreen; color: white; }
</style>
```

Üç Parçalı(SFC) mimarinin Getirdiği O Saf "Mühendislik Düzeni"; React'in kod çorbasında hüsrana boğulmuş Orta-(Ve Solo/Kendi kendine şirket kuran) Geliştiricilerin adeta cennetidir. Ayrıca Vite'in de Yaratıcısı yine Evan You'dur.

## Kimler Kullanır?
* Türkiye'de, Avrupa'da ve Özellikle Çin Piyasasında; React'ın tek gerçek, sevimli (Ve hatta geliştirici anketi memnuniyetlerinde en Yüksek olan) **Vue.js FrontEnd Yazılımcılarıdır**.
* Eski Tip PHP (Laravel Ecosystem) Geliştiricileri. Laravel (Taylor Otwell) resmi olarak Vue.js ile aşk yaşadığını (Entegrasyonu desteklediğini) duyurduğundan Beri, PHP yazılımcılarının Frontend'e geçerken kullandığı O muazzam kılıçtır. 
* Tıpkı React -> Next.js evrimi gibi; **Vue.js -> Nuxt.js** adında bir Meta-Framework evrimi geçirmiş olup (SEO ve SSR Renderları) o tarafa aktarmıştır. Çoğu kişi React'tan daha basit ve "Kurulumu Temiz" olduğu için projelerine Aşamalı (Progresive) Vue takar.
